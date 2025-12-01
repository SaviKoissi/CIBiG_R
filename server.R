library(shiny)
library(readr)
library(openxlsx)
library(dplyr)
library(shinyjs)

shinyServer(function(input, output, session) {
  
  #=====================================================
  # TIMER — 1 hour (3600 seconds)
  #=====================================================
  total_time <- 3600  # 1 hour
  rv <- reactiveValues(time_left = total_time)
  
  autoInvalidate <- reactiveTimer(1000)  # Update every second
  
  observe({
    autoInvalidate()
    rv$time_left <- rv$time_left - 1
    
    mins <- sprintf("%02d", rv$time_left %/% 60)
    secs <- sprintf("%02d", rv$time_left %% 60)
    
    output$timer <- renderText(
      paste0("⏱ Remaining time: ", mins, ":", secs)
    )
    
    # If time runs out → auto-submit
    if (rv$time_left <= 0) {
      isolate({
        submit_quiz(auto = TRUE)
      })
    }
  })
  
  
  #=====================================================
  # Load MCQs from GitHub
  #=====================================================
  url_mcq <- "https://raw.githubusercontent.com/SaviKoissi/CIBiG_R/main/questions_mcq.csv"
  
  mcq_df <- read_csv(url_mcq, show_col_types = FALSE)
  
  
  #=====================================================
  # Build MCQ UI
  #=====================================================
  output$mcq_ui <- renderUI({
    lapply(seq_len(nrow(mcq_df)), function(i) {
      q <- mcq_df[i, ]
      
      wellPanel(
        h4(paste0(q$id, ". ", q$question)),
        radioButtons(
          inputId = q$id,
          label = "Choose:",
          choices = as.character(q[, c("choice1","choice2","choice3","choice4")])
        )
      )
    })
  })
  
  
  #=====================================================
  # Code Questions
  #=====================================================
  code_questions <- list(
    C1 = "Write R code to compute GC content of 'ATGCGC'.",
    C2 = "Given counts <- c(10,5,8), write code to compute its mean."
  )
  
  output$code_ui <- renderUI({
    lapply(names(code_questions), function(id) {
      wellPanel(
        h4(code_questions[[id]]),
        textAreaInput(id, "Your R code:", width = "100%", height = "120px")
      )
    })
  })
  
  
  #=====================================================
  # Safe evaluate function
  #=====================================================
  safe_eval <- function(code) {
    tryCatch({
      val <- eval(parse(text = code))
      list(ok = TRUE, value = val)
    }, error = function(e) {
      list(ok = FALSE, value = NA)
    })
  }
  
  
  #=====================================================
  # QUIZ SUBMISSION FUNCTION (manual + auto)
  #=====================================================
  submit_quiz <- function(auto = FALSE) {
    
    student_name <- ifelse(is.null(input$student) || input$student == "",
                           "Unknown Student",
                           input$student)
    
    #-------------------------------------------
    # MCQ Grading
    #-------------------------------------------
    mcq_score <- 0
    total_mcq <- nrow(mcq_df)
    
    for (i in seq_len(total_mcq)) {
      q <- mcq_df[i, ]
      if (!is.null(input[[q$id]]) && input[[q$id]] == q$answer) {
        mcq_score <- mcq_score + 1
      }
    }
    
    
    #-------------------------------------------
    # Code Question Grading (safe)
    #-------------------------------------------
    code_score <- 0
    total_code <- length(code_questions)
    
    code_rules <- list(
      C1 = c("sum", "%in%", "nchar"),
      C2 = c("mean")
    )
    
    for (id in names(code_questions)) {
      
      ans <- input[[id]]
      
      if (is.null(ans) || ans == "") {
        next
      }
      
      required <- code_rules[[id]]
      
      matches <- sapply(required, function(k) grepl(k, ans, fixed = TRUE))
      
      if (isTRUE(any(matches))) {
        code_score <- code_score + 1
      }
    }
    
    
    total_score <- mcq_score + code_score
    total_possible <- total_mcq + total_code
    
    
    #-------------------------------------------
    # Save to Excel
    #-------------------------------------------
    file <- "results.xlsx"
    
    new_entry <- data.frame(
      student = student_name,
      mcq_score = mcq_score,
      code_score = code_score,
      total = total_score,
      total_possible = total_possible,
      timestamp = as.character(Sys.time())
    )
    
    if (!file.exists(file)) {
      write.xlsx(new_entry, file)
    } else {
      old <- read.xlsx(file)
      write.xlsx(rbind(old, new_entry), file)
    }
    
    
    #-------------------------------------------
    # Display result and exit
    #-------------------------------------------
    output$result <- renderText(
      paste0("Score for ", student_name, ": ",
             total_score, "/", total_possible,
             ". Saved to Excel. The quiz will now close.")
    )
    
    # Quit app after short delay
    shinyjs::delay(1500, shiny::stopApp())
  }
  
  
  #=====================================================
  # Manual SUBMIT button
  #=====================================================
  observeEvent(input$submit, {
    submit_quiz(auto = FALSE)
  })
  
})
