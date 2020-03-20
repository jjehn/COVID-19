library(shiny)

library (readr)

fluidPage(
  plotOutput(outputId = "timeseries"),
  "Toggle the following parameters to get an understanding why restrictions such as social distancing are life-saving:",
  sliderInput(inputId = "deathrate", 
              label = "Set estimated death rate", 
              value = 1,
              min = 0.01, max = 5),
  checkboxInput(inputId = "restrictions", 
                label = "Calculate without effective restrictions?", 
                value = F),
  uiOutput("slider_control"),
  plotOutput(outputId = "barplot"),
  "\n\nThe following plot shows the expected number of COVID-19 deaths on basis of the doubling time of the last 15 days:",
  plotOutput(outputId = "estdevelopment"),
  "Check out the code behind this App: ",
  tags$a(href="https://github.com/jjehn/COVID-19", "https://github.com/jjehn/COVID-19")
)