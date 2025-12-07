# server.R 
library(shiny)
library(readr)
library(dplyr)
library(shinyjs)
library(blastula)

shinyServer(function(input, output, session) {
  
  
  # -----------------------------
  # 0. Helpers
  # -----------------------------
  empty_results_df <- function() {
    data.frame(
      student = character(),
      mcq_score = integer(),
      code_score = integer(),
      total = integer(),
      total_possible = integer(),
      timestamp = character(),
      stringsAsFactors = FALSE
    )
  }
  
  # # -----------------------------
  # # EMAIL SENDING FUNCTION
  # # -----------------------------
  # send_result_email <- function(student_name, mcq_score, code_score, total_score, total_possible, code_answers) {
  #   tryCatch({
  #     # Create email body
  #     email_body <- paste0(
  #       "Quiz Results\n",
  #       "====================\n\n",
  #       "Student: ", student_name, "\n",
  #       "Timestamp: ", Sys.time(), "\n\n",
  #       "Scores:\n",
  #       "-------\n",
  #       "MCQ Score: ", mcq_score, "/", nrow(mcq_df), "\n",
  #       "Code Score: ", code_score, "/", length(code_questions), "\n",
  #       "Total Score: ", total_score, "/", total_possible, "\n\n",
  #       "Code Answers:\n",
  #       "-------------\n"
  #     )
  # 
  #     # Append code answers
  #     for (id in names(code_questions)) {
  #       ans <- code_answers[[id]]
  #       if (is.null(ans)) ans <- "[No answer provided]"
  #       email_body <- paste0(
  #         email_body,
  #         "\n", id, ": ", code_questions[[id]], "\n",
  #         "Answer:\n", ans, "\n",
  #         "-------------------\n"
  #       )
  #     }
  # 
  #     # Create email
  #     email <- compose_email(
  #       body = md(paste0("```\n", email_body, "\n```"))
  #     )
  # 
  #     # Send email
  #     smtp_send(
  #       email,
  #       from = cfg$user,
  #       to = cfg$user,
  #       subject = paste0("CiBIGR Quiz Result - ", student_name, " - ", format(Sys.time(), "%Y-%m-%d %H:%M")),
  #       credentials = cfg
  #     )
  # 
  #     return(TRUE)
  #   }, error = function(e) {
  #     warning("Failed to send email: ", e$message)
  #     return(FALSE)
  #   })
  # }
  
  
  # -----------------------------
  # EMAIL SENDING FUNCTION
  # -----------------------------
  send_result_email <- function(student_name, mcq_score, code_score, total_score, total_possible, code_answers) {
    tryCatch({
      # Create email body header
      email_body <- paste0(
        "Quiz Results\n",
        "====================\n\n",
        "Student: ", student_name, "\n",
        "Timestamp: ", Sys.time(), "\n\n",
        "Scores:\n",
        "-------\n",
        "MCQ Score: ", mcq_score, "/", nrow(mcq_df), "\n",
        "Code Score: ", code_score, "/", length(code_questions), "\n",
        "Total Score: ", total_score, "/", total_possible, "\n\n",
        "Code Answers:\n",
        "-------------\n"
      )
      
      # ===== Add MCQ detailed info here =====
      email_body <- paste0(email_body, "\nMCQ Details:\n-------------\n")
      for (i in seq_len(nrow(mcq_df))) {
        qid <- mcq_df$id[i]
        question_text <- mcq_df$question[i]
        correct_answer <- mcq_df$answer[i]
        
        # Attempt to get the student's answer from input if possible, else "[No answer]"
        student_answer <- if (!is.null(input[[qid]])) input[[qid]] else "[No answer provided]"
        
        correctness <- if (identical(as.character(student_answer), as.character(correct_answer))) {
          "✔ Correct"
        } else {
          "✘ Incorrect"
        }
        
        email_body <- paste0(
          email_body,
          "Q", i, " (", qid, "): ", question_text, "\n",
          "Your answer: ", student_answer, "\n",
          "Correct answer: ", correct_answer, "\n",
          "Result: ", correctness, "\n\n"
        )
      }
      
      # ===== Add code question detailed info here =====
      email_body <- paste0(email_body, "\nCode Question Details:\n-----------------------\n")
      # Assume code_rules is globally accessible for this; if not, must add param (but you requested no change)
      for (id in names(code_questions)) {
        question_text <- code_questions[[id]]
        ans <- code_answers[[id]]
        if (is.null(ans) || trimws(ans) == "") ans <- "[No answer provided]"
        
        # Attempt to get required keywords from code_rules global object
        required_keywords <- NULL
        if (exists("code_rules")) {
          required_keywords <- code_rules[[id]]
        }
        
        # Build keyword match details if available
        if (!is.null(required_keywords)) {
          matches <- sapply(required_keywords, function(k) grepl(k, ans, fixed = FALSE))
          keywords_status <- paste0(
            paste(required_keywords, ifelse(matches, "✔ found", "✘ missing"), collapse = "\n"),
            "\n"
          )
          summary_line <- if (any(matches)) "✔ Required elements detected" else "✘ Required elements missing"
        } else {
          keywords_status <- "No keyword rules found for this question.\n"
          summary_line <- ""
        }
        
        email_body <- paste0(
          email_body,
          "Code Question (", id, "): ", question_text, "\n",
          "Your code:\n", ans, "\n\n",
          "Required keywords:\n", keywords_status, "\n",
          "Summary: ", summary_line, "\n\n"
        )
      }
      
      # Create email
      email <- compose_email(
        body = md(paste0("```\n", email_body, "\n```"))
      )
      
      # Send email
      smtp_send(
        email,
        from = cfg$user,
        to = cfg$user,
        subject = paste0("CIBiR R Quiz Result - ", student_name, " - ", format(Sys.time(), "%Y-%m-%d %H:%M")),
        credentials = cfg
      )
      
      return(TRUE)
    }, error = function(e) {
      warning("Failed to send email: ", e$message)
      return(FALSE)
    })
  }
  
  
  # -----------------------------
  # 1. Timer configuration (1 hour)
  # -----------------------------
  total_time <- 3600
  rv <- reactiveValues(
    time_left = total_time,
    submitted = FALSE
  )
  
  # -----------------------------
  # 2. Load MCQ data safely
  # -----------------------------
  url_mcq <- "https://raw.githubusercontent.com/SaviKoissi/CiBiGR/main/questions_mcq.csv"
  
  mcq_df <- tryCatch({
    as.data.frame(read_csv(url_mcq, show_col_types = FALSE), stringsAsFactors = FALSE)
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error loading questions",
      paste0("Could not read MCQ file at:\n", url_mcq, "\n\n", e$message),
      easyClose = TRUE
    ))
    data.frame(id = "missing",
               question = "Questions file could not be loaded. Contact the instructor.",
               choice1 = "N/A", choice2 = "N/A", choice3 = "N/A", choice4 = "N/A",
               answer = NA, stringsAsFactors = FALSE)
  })
  
  # Defensive: ensure mcq_df has expected columns
  required_cols <- c("id", "question", "choice1", "choice2", "choice3", "choice4", "answer")
  for (rc in setdiff(required_cols, names(mcq_df))) {
    mcq_df[[rc]] <- NA
  }
  
  # Ensure id is character and unique
  mcq_df$id <- as.character(mcq_df$id)
  mcq_df$id <- paste0("q_", make.names(mcq_df$id))
  
  # -----------------------------
  # 3. Render MCQ UI
  # -----------------------------
  output$mcq_ui <- renderUI({
    req(mcq_df)
    panels <- lapply(seq_len(nrow(mcq_df)), function(i) {
      q <- mcq_df[i, ]
      qid <- q$id
      wellPanel(
        h4(paste0(gsub("^q_", "", qid), ". ", q$question)),
        radioButtons(
          inputId = qid,
          label = NULL,
          choices = na.omit(as.character(c(q$choice1, q$choice2, q$choice3, q$choice4))),
          selected = character(0)
        )
      )
    })
    do.call(tagList, panels)
  })
  
  # -----------------------------
  # 4. Code Questions (static)
  # -----------------------------
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
  
  # -----------------------------
  # 5. Submission function (with email)
  # -----------------------------
  submit_quiz <- function(auto = FALSE) {
    if (isTRUE(rv$submitted)) return(invisible(NULL))
    rv$submitted <- TRUE
    
    # Sanitize student name
    student_name <- input$student
    if (is.null(student_name) || trimws(as.character(student_name)) == "") {
      student_name <- "Unknown Student"
    }
    student_name <- as.character(student_name)
    
    # MCQ scoring
    mcq_score <- 0L
    if (nrow(mcq_df) > 0) {
      for (i in seq_len(nrow(mcq_df))) {
        q <- mcq_df[i, ]
        qid <- q$id
        ans <- NULL
        if (!is.null(input[[qid]])) ans <- input[[qid]]
        if (!is.null(ans) && !is.na(q$answer) && as.character(ans) == as.character(q$answer)) {
          mcq_score <- mcq_score + 1L
        }
      }
    }
    
    # Code scoring
    code_score <- 0L
    code_rules <- list(
      C1 = c("sum", "%in%", "nchar"),
      C2 = c("mean")
    )
    
    # Collect code answers
    code_answers <- list()
    for (id in names(code_questions)) {
      ans <- input[[id]]
      code_answers[[id]] <- if (is.null(ans) || trimws(as.character(ans)) == "") {
        "[No answer provided]"
      } else {
        as.character(ans)
      }
      
      if (!is.null(ans) && trimws(as.character(ans)) != "") {
        required <- code_rules[[id]]
        matches <- sapply(required, function(k) {
          grepl(k, ans, fixed = FALSE)
        })
        if (any(matches)) code_score <- code_score + 1L
      }
    }
    
    total_mcq <- nrow(mcq_df)
    total_code <- length(code_questions)
    total_score <- as.integer(mcq_score + code_score)
    total_possible <- as.integer(total_mcq + total_code)
    
    # Send email with results
    email_sent <- send_result_email(
      student_name, 
      mcq_score, 
      code_score, 
      total_score, 
      total_possible,
      code_answers
    )
    
    # Show result to the user
    output$result <- renderText({
      if (email_sent) {
        paste0(
          "Score for ", student_name, ": ",
          total_score, "/", total_possible,
          ". Results sent to instructor!"
        )
      } else {
        paste0(
          "Score for ", student_name, ": ",
          total_score, "/", total_possible,
          ". WARNING: Email failed to send. Please contact instructor."
        )
      }
    })
    
    # UI toggles
    shinyjs::hide("submit")
    shinyjs::show("quit")
    
    if (isTRUE(auto)) {
      session$onFlushed(function() {
        stopApp()
      }, once = TRUE)
    }
    
    invisible(NULL)
  }
  
  # -----------------------------
  # 6. Observers: manual submit & quit
  # -----------------------------
  observeEvent(input$submit, {
    if (isTRUE(rv$submitted)) return()
    submit_quiz(auto = FALSE)
  }, ignoreInit = TRUE)
  
  observeEvent(input$quit, {
    stopApp()
  }, ignoreInit = TRUE)
  
  # -----------------------------
  # 7. Timer loop (1s)
  # -----------------------------
  observe({
    invalidateLater(1000, session)
    req(mcq_df)
    timer$stop("quizz")
    output$timer <- renderText({
      time_left <- total_time - getTimer(timer)$timeElapsed
      if(time_left > 60) {
        paste(round(time_left/60, 0), "minutes remaining...")
      } else {
        paste(time_left, "seconds remaining...")
      }
    })
    
    if (getTimer(timer)$timeElapsed >= total_time) {
      showModal(modalDialog(
        title = "Time Over!",
        "Time expired - auto submitting quiz...",
        footer = NULL
      ))
      Sys.sleep(5)
      isolate(submit_quiz(auto = TRUE))
      stopApp()
    }
  })
  #  
  # -----------------------------
  # 8. UI initial state
  # -----------------------------
  shinyjs::hide("quit")
})