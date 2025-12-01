library(shiny)
library(readr)
library(openxlsx)
library(dplyr)

shinyServer(function(input, output, session) {
  
  #---------------------------
  # Load MCQ questions from GitHub
  #---------------------------
url_mcq <- "https://raw.githubusercontent.com/SaviKoissi/CIBiG_R/main/questions_mcq.csv"

mcq_df <- readr::read_csv(url_mcq, show_col_types = FALSE)

  mcq_df <- read_csv(url_mcq, show_col_types = FALSE)
  
  #---------------------------
  # Build UI for MCQs dynamically
  #---------------------------
  output$mcq_ui <- renderUI({
    lapply(seq_len(nrow(mcq_df)), function(i) {
      q <- mcq_df[i, ]
      
      wellPanel(
        h4(paste0(q$id, ". ", q$question)),
        radioButtons(
          inputId = q$id,
          label = "Choose:",
          choices = as.character(q[ , c("choice1","choice2","choice3","choice4")])
        )
      )
    })
  })
  
  #---------------------------
  # Code questions (static or from GitHub)
  #---------------------------
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
  
  #---------------------------
  # Evaluate code safely
  #---------------------------
  safe_eval <- function(code) {
    tryCatch({
      val <- eval(parse(text = code))
      list(ok = TRUE, value = val)
    }, error = function(e) {
      list(ok = FALSE, value = NA)
    })
  }
  
  #---------------------------
  # Grading logic
  #---------------------------
  observeEvent(input$submit, {
    
    if (input$student == "") {
      output$result <- renderText("Please enter your name before submitting.")
      return()
    }
    
    # MCQ grading
    mcq_score <- 0
    total_mcq <- nrow(mcq_df)
    
    for (i in seq_len(total_mcq)) {
      q <- mcq_df[i, ]
      if (!is.null(input[[q$id]]) && input[[q$id]] == q$answer) {
        mcq_score <- mcq_score + 1
      }
    }
    
    # Code question grading (simple keyword presence or dummy grading)
    code_score <- 0
    total_code <- length(code_questions)
    
    # Example simple grading rules:
    # C1: must include "sum" or "%in%" or both
    # C2: must include "mean"
    code_rules <- list(
      C1 = c("sum", "%in%", "nchar"),
      C2 = c("mean")
    )
    
    for (id in names(code_questions)) {
      ans <- input[[id]]
      required <- code_rules[[id]]
      
      # check if any required keyword appears
      if (any(sapply(required, function(k) grepl(k, ans, fixed = TRUE)))) {
        code_score <- code_score + 1
      }
    }
    
    total_score <- mcq_score + code_score
    total_possible <- total_mcq + total_code
    
    #---------------------------
    # Save to Excel
    #---------------------------
    file <- "results.xlsx"
    
    new_entry <- data.frame(
      student = input$student,
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
    
    # Display result
    output$result <- renderText(
      paste0("Score submitted for ", input$student,
             ": ", total_score, " / ", total_possible,
             ". Saved to Excel.")
    )
  })
})
