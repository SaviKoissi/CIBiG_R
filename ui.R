library(shiny)

shinyUI(fluidPage(
  
  titlePanel("Bioinformatics Live Quiz"),
  
  textInput("student", "Enter your full name:"),
  
  h3("Multiple-choice questions"),
  uiOutput("mcq_ui"),
  
  h3("Code questions (write R code)"),
  uiOutput("code_ui"),
  
  actionButton("submit", "Submit Answers", class = "btn-primary"),
  br(), br(),
  textOutput("result")
))

