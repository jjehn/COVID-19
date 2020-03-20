## load required libraries

library (readr)

library(shiny)

library(rsconnect)

## Use shiny to create a shiny app

# idea: https://shiny.rstudio.com/gallery/covid19-tracker.html

ui <- fluidPage(
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

server <- function(input, output) {
  
    output$slider_control = renderUI({
    if (input$restrictions == F) {
      return(NULL)
    }
    else {
      sliderInput(inputId = "doublingtime", 
                  label = "Set estimated doubling time", 
                  value = 5,
                  min = 2, max = 20)
    }
  })
    
  
  output$table <- renderPlot(
    { ## load data of COVID-19 deaths from Johns Hopkins GitHub
      urlfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
      deaths<-read.csv(url(urlfile))
      head(deaths)
      
      ## reformate data
      # get column number of today's data
      latest <- ncol(deaths)
      # reduce deaths to country and death numbers of today day
      deaths_data <- deaths[,c(2,latest)]
      # rename columns
      colnames(deaths_data) <- c("Country.Region", "deaths_today")
      # aggregate regional death numbers countrywise
      deaths_data <- aggregate(. ~ Country.Region, deaths_data, sum)
      head(deaths_data)
      # estimate number of infections 15.3 days ago based on deaths of today
      deaths_data$infected_15.3D_bd <- deaths_data$deaths_today*(100/input$deathrate)
      
      # in case of restrictions do not apply doubling factor
      if (input$restrictions == F) { 
        deaths_data$infected_today <- deaths_data$infected_15.3D_bd
        }
      else {
        # estimate number of actual infections based on doubling factor added on 15.3 days before infections
        deaths_data$infected_today <- deaths_data$infected_15.3D_bd*2^(15.3/input$doublingtime)
        }
      
      # sort countries by from most infected to least infectes and reduce to 10 (China is left out as restrictions seem to be successful)
      most_infected_countries <- head(deaths_data[order(deaths_data$infected_today, decreasing = T),],11)
      most_infected_countries <- most_infected_countries[-grep("China", most_infected_countries$Country.Region),]
      
      # generate barplot
      barplot(most_infected_countries$infected_today, 
              main="Estimate of infected people up to date", horiz=F,
              names.arg = most_infected_countries$Country.Region)
      }
  )
}

shinyApp(ui = ui, server = server)
