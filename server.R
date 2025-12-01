library(shiny)
library(readr)
library(openxlsx)
library(dplyr)

shinyServer(function(input, output, session) {
  
  #=====================================================
  # TIMER: 1 hour (3600 seconds)
  #=====================================================
  total_time <- 3600   # 1 hour
  rv <- reactiveValues(time_left = total_time)
  
  # Update timer every second
  autoInvalidate <- reactiveTimer(1000)
  
  observe({
    autoInvalidate()
    rv$time_left <- rv$time_left - 1
    
    # Display remaining time in mm:ss
    mins <- sprintf("%02d", rv$time_left %/% 60)
    secs <- sprintf("%02d", rv$time_left %% 60)
    output$timer <- renderText(paste0("⏱ Remaining time: ", mins, ":", secs))
    
    # If timer ends → autosubmit + quit
    if (rv$time_left <= 0) {
      isolate({
        submit_quiz(auto = TRUE)
      })
    }
  })
  
  #=====================================================
  # Load MCQ Questions
  #=====================================================
  url_mcq <- "https://raw.githubusercontent.com/SaviKoissi/CIBiG_R/main/questions_mcq.csv"
  mcq_df <- readr::read_csv(url_mcq, show_col_types = FALSE)
  
  #=====================================================
  # Render MCQs
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
  # Safe Evaluation Function
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
  # QUIZ SUBMISSION FUNCTION (manual or auto)
  #=====================================================
  submit_quiz <- function(auto = FALSE) {
    
    student_name <- ifelse(is.null(input$student) || input$student == "",
                           "Unknown Student", input$student)
    
    # MCQ Scoring
    mcq_score <- 0
    total_mcq <- nrow(mcq_df)
    
    for (i in seq_len(total_mcq)) {
      q <- mcq_df[i, ]
      if (!is.null(input[[q$id]]) && input[[q$id]] == q$answer) {
        mcq_score <- mcq_score + 1
      }
    }
    
    # Code scoring
    code_score <- 0
    total_code <- length(code_questions)
    
    code_rules <- list(
      C1 = c("sum", "%in%", "nchar"),
      C2 = c("mean")
    )
    
    for (id in names(code_questions)) {
      ans <- input[[id]]
      required <- code_rules[[id]]
      if (any(sapply(required, function(k) grepl(k, ans, fixed = TRUE)))) {
        code_score <- code_score + 1
      }
    }
    
    total_score <- mcq_score + code_score
    total_possible <- total_mcq + total_code
    
    # Save to Excel
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
    
    # Display message
    output$result <- renderText(
      paste0("Score for ", student_name, ": ",
             total_score, "/", total_possible,
             ". Saved to Excel. The quiz will now close.")
    )
    
    # Quit the app 1 second later
    shinyjs::delay(1000, shiny::stopApp())
  }
  
  #=====================================================
  # Manual submission button
  #=====================================================
  observeEvent(input$submit, {
    submit_quiz(auto = FALSE)
  })
})
