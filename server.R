# server.R 
library(shiny)
library(readr)
library(openxlsx)
library(dplyr)
library(shinyjs)

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
  
  coerce_old_results <- function(old) {
    # If old is NULL or invalid, return empty standard DF
    if (is.null(old) || (!is.data.frame(old))) return(empty_results_df())
    
    # Ensure expected columns exist; add missing as NA
    expected <- names(empty_results_df())
    missing_cols <- setdiff(expected, names(old))
    if (length(missing_cols) > 0) {
      for (mc in missing_cols) old[[mc]] <- NA
    }
    
    # Keep only expected columns in order
    old <- old[, expected, drop = FALSE]
    
    # Coerce types defensively
    old$student <- as.character(old$student)
    old$mcq_score <- suppressWarnings(as.integer(as.numeric(old$mcq_score)))
    old$code_score <- suppressWarnings(as.integer(as.numeric(old$code_score)))
    old$total <- suppressWarnings(as.integer(as.numeric(old$total)))
    old$total_possible <- suppressWarnings(as.integer(as.numeric(old$total_possible)))
    old$timestamp <- as.character(old$timestamp)
    
    # Replace any NA in numeric columns with 0 (optional - keep NA if you prefer)
    old$mcq_score[is.na(old$mcq_score)] <- 0L
    old$code_score[is.na(old$code_score)] <- 0L
    old$total[is.na(old$total)] <- 0L
    old$total_possible[is.na(old$total_possible)] <- 0L
    
    return(old)
  }
  
  safe_bind_rows <- function(a, b) {
    # Try regular bind_rows, fallback to character coercion
    out <- tryCatch({
      bind_rows(a, b)
    }, error = function(e) {
      # convert all columns to character to ensure bind works
      a2 <- a
      b2 <- b
      a2[] <- lapply(a2, function(x) as.character(x))
      b2[] <- lapply(b2, function(x) as.character(x))
      bind_rows(a2, b2)
    })
    return(out)
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
  url_mcq <- "https://raw.githubusercontent.com/SaviKoissi/CIBiG_R/main/questions_mcq.csv"
  
  mcq_df <- tryCatch({
    # read_csv returns a tibble; coerce to data.frame for stable indexing
    as.data.frame(read_csv(url_mcq, show_col_types = FALSE), stringsAsFactors = FALSE)
  }, error = function(e) {
    # Do NOT stop the app here; show friendly modal and provide fallback empty MCQ DF
    showModal(modalDialog(
      title = "Error loading questions",
      paste0("Could not read MCQ file at:\n", url_mcq, "\n\n", e$message),
      easyClose = TRUE
    ))
    # return a minimal dataframe so UI can render a friendly message
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
  
  # Ensure id is character and unique (prefix to avoid invalid inputId like numeric)
  mcq_df$id <- as.character(mcq_df$id)
  mcq_df$id <- paste0("q_", make.names(mcq_df$id))
  
  # -----------------------------
  # 3. Render MCQ UI
  # -----------------------------
  output$mcq_ui <- renderUI({
    req(mcq_df)
    # If mcq_df contains just the fallback "missing" row, user sees message
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
  # 5. Submission function (robust)
  # -----------------------------
  results_file <- "/Users/koissi/Desktop/WAVE_project/CBIG/IntroR_Bioinfo/results.xlsx"

  
  submit_quiz <- function(auto = FALSE) {
    # Prevent double-submit in this R session
    if (isTRUE(rv$submitted)) return(invisible(NULL))
    rv$submitted <- TRUE
    
    # sanitize student name
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
        # protect against missing input
        ans <- NULL
        if (!is.null(input[[qid]])) ans <- input[[qid]]
        # compare as character; NA answers will be treated as incorrect
        if (!is.null(ans) && !is.na(q$answer) && as.character(ans) == as.character(q$answer)) {
          mcq_score <- mcq_score + 1L
        }
      }
    }
    
    # Code scoring (simple keyword-based)
    code_score <- 0L
    code_rules <- list(
      C1 = c("sum", "%in%", "nchar"),
      C2 = c("mean")
    )
    for (id in names(code_questions)) {
      ans <- input[[id]]
      if (is.null(ans) || trimws(as.character(ans)) == "") next
      required <- code_rules[[id]]
      matches <- sapply(required, function(k) {
        grepl(k, ans, fixed = FALSE)
      })
      if (any(matches)) code_score <- code_score + 1L
    }
    
    total_mcq <- nrow(mcq_df)
    total_code <- length(code_questions)
    total_score <- as.integer(mcq_score + code_score)
    total_possible <- as.integer(total_mcq + total_code)
    
    # new entry (guarantee types)
    new_entry <- data.frame(
      student = as.character(student_name),
      mcq_score = as.integer(mcq_score),
      code_score = as.integer(code_score),
      total = as.integer(total_score),
      total_possible = as.integer(total_possible),
      timestamp = as.character(Sys.time()),
      stringsAsFactors = FALSE
    )
    
    # read old safely (if exists), coerce types, combine safely
    old <- tryCatch({
      if (!file.exists(results_file)) NULL else read.xlsx(results_file)
    }, error = function(e) {
      # show a non-fatal message in console; keep app alive
      message("Warning: couldn't read existing results.xlsx; proceeding with empty table. ", e$message)
      NULL
    })
    
    old_safe <- coerce_old_results(old)
    combined <- safe_bind_rows(old_safe, new_entry)
    
    # write safely to temporary file then replace
    tmpf <- paste0(results_file, ".tmp")
    tryCatch({
      write.xlsx(combined, tmpf)
      # attempt atomic move
      if (!file.rename(tmpf, results_file)) {
        # fallback: try remove + rename
        if (file.exists(results_file)) file.remove(results_file)
        file.rename(tmpf, results_file)
      }
    }, error = function(e) {
      warning("Failed to write results.xlsx: ", e$message)
    })
    
    # show result to the user
    output$result <- renderText({
      paste0(
        "Score for ", student_name, ": ",
        total_score, "/", total_possible,
        ". Saved."
      )
    })
    
    # UI toggles: hide submit, show quit
    shinyjs::hide("submit")
    shinyjs::show("quit")
    
    # if auto (timer), close after flush (small safe delay is okay)
    if (isTRUE(auto)) {
      session$onFlushed(function() {
        # stopApp will end the server; we only call it after UI has sent the final message
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
  # observe({
  #   req(mcq_df)
  #   timer()
  #   message("Timer tick: time_left = ", rv$time_left)
  #   
  #   if (isTRUE(rv$submitted)) {
  #     message("Timer stopped: already submitted")
  #     return()
  #   }
  #   
  #   rv$time_left <- max(0, rv$time_left - 1)
  #   
  #   if (rv$time_left == 0 && !isTRUE(rv$submitted)) {
  #     message("Time expired - auto submitting quiz")
  #     isolate(submit_quiz(auto = TRUE))
  #   }
  # })
  # 
  
  # -----------------------------
  # 8. UI initial state
  # -----------------------------
  shinyjs::hide("quit")
})
