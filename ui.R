library(shiny)
library(shinyjs)
library(timeR)

timer <<- timeR::createTimer(precision = "s")
timer$start("quizz")
timer$toggleVerbose()


setwd(paste0(getwd()))
cfg <<- readRDS("smtp.cfg")


fluidPage(
  useShinyjs(),         
  
  tags$style(HTML("
    #timer {
      position: fixed;
      top: 0;
      left: 0;
      right: 0;
      background-color: #f8f9fa;
      padding: 15px;
      border-bottom: 2px solid #dee2e6;
      z-index: 1000;
      text-align: center;
      font-size: 18px;
      font-weight: bold;
      box-shadow: 0 2px 4px rgba(0,0,0,0.1);
    }
    
    /* Add padding to the body to prevent content from hiding under the fixed timer */
    body {
      padding-top: 60px;
    }
  ")),
  
  textOutput("timer"),
  textInput("student", "Enter your full name:"),
  
  uiOutput("mcq_ui"),
  uiOutput("code_ui"),
  
  actionButton("submit", "Submit Answers"),
  actionButton("quit", "Quit", style = "display:none;"),
  br(),
  textOutput("result")
)
