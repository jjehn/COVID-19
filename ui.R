library(shiny)

library (readr)

fluidPage(
  sliderInput(inputId = "deathrate", 
              label = "Set estimated death rate", 
              value = 1,
              min = 0.01, max = 5),
  checkboxInput(inputId = "restrictions", 
                label = "Calculate without effective restrictions?", 
                value = F),
  uiOutput("slider_control"),
  plotOutput(outputId = "table")
)