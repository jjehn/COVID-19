library (readr)

library(ggplot2)

library(reshape2)

library(shiny)

library(rsconnect)

function(input, output) {
  
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
  
  
  output$barplot <- renderPlot(
    { ## load data of COVID-19 deaths from Johns Hopkins GitHub
      urlfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
      deaths<-read.csv(url(urlfile))
      head(deaths)
      
      ## reformate data for estimate of infections up to date
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
      
      # generate basic barplot
      #barplot(most_infected_countries$infected_today, 
      #        main="Estimate of infected people up to date", horiz=F,
      #       names.arg = most_infected_countries$Country.Region)
      
      # generate barplot with ggplot
      ggplot(most_infected_countries, aes(x=Country.Region, y=infected_today)) +
        geom_bar(stat="identity") +
        ggtitle("Estimate of COVID-19 infected people up to date") +
        xlab("Country") + ylab("Estimated number of infections")
    }
  )
  
  
  output$timeseries <- renderPlot(
    { ## load data of COVID-19 deaths from Johns Hopkins GitHub
      urlfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
      deaths<-read.csv(url(urlfile))
      head(deaths)
      
      ## reformate data to plot time series
      # remove regions and location information
      deaths_timeseries <- deaths[,-c(1,3,4)]
      # aggregate regional death numbers countrywise
      deaths_timeseries <- aggregate(. ~ Country.Region, deaths_timeseries, sum)
      # melt to have data in ggplot formate
      meltdf <- melt(deaths_timeseries,id="Country.Region")
      head(meltdf)
      # remove X in month variable
      meltdf$variable <- substring(meltdf$variable, 2)
      # select only countries of interest for plot
      today <- ncol(deaths_timeseries)
      most_infected_countries <- deaths_timeseries[,c(1,today)]
      colnames(most_infected_countries) <- c("Country.Region", "today")
      most_infected_countries <- head(most_infected_countries[order(most_infected_countries$today, decreasing = T),],11)
      countriesOI <- most_infected_countries$Country.Region
      meltdf_most_infected <- meltdf[meltdf$Country.Region %in% countriesOI,]
      # plot time series graph for countries of interest of cumulative deaths
      ggplot(meltdf_most_infected,
             aes(x=factor(variable, levels = unique(variable)),
                 y=value,
                 colour=Country.Region,
                 group=Country.Region)
      ) + geom_line() +
        ggtitle("COVID-19 deaths in most infected countries") +
        xlab("Date") + ylab("Cumulative Deaths")
      
      # show data for just one country
      #meltdf[grep("China", meltdf$Country.Region),]
      
    }
  )
  
  
  output$estdevelopment <- renderPlot(
    { ## load data of COVID-19 deaths from Johns Hopkins GitHub
      urlfile <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
      deaths<-read.csv(url(urlfile))
      head(deaths)
      
      ## reformate data to calculate latest doubling time
      # get column number of today's data
      latest <- ncol(deaths)
      # get column number of 14.3 days ago
      pre14.3D <- latest-14
      # remove regions and location information
      estdevdata_data <- deaths[,-c(1,3,4)]
      # aggregate regional death numbers countrywise
      estdevdata_data <- aggregate(. ~ Country.Region, estdevdata_data, sum)
      head(estdevdata_data)
      # calculate latest doubling time
      doublingtime <- 14.3/log2(estdevdata_data[,latest-3]/estdevdata_data[,pre14.3D-3])
      # create subset of time series starting 13 days ago
      estdevdata_data_subset <- estdevdata_data[,-(2:(pre14.3D-4))]
      rownames(estdevdata_data_subset) <- estdevdata_data_subset$Country.Region
      estdevdata_data_subset <- estdevdata_data_subset[,-1]
      estdevdata_data_prognosis <- estdevdata_data_subset*2^(14.3/doublingtime)
      head(estdevdata_data_prognosis)
      estdevdata_data_prognosis$Country.Region <- rownames(estdevdata_data_prognosis)
      
      # melt to have data in ggplot formate
      meltdf <- melt(estdevdata_data_prognosis,id="Country.Region")
      head(meltdf)
      # remove X in month variable
      meltdf$variable <- substring(meltdf$variable, 2)
      # select only countries of interest for plot
      today <- ncol(estdevdata_data)
      most_infected_countries <- estdevdata_data[,c(1,today)]
      colnames(most_infected_countries) <- c("Country.Region", "today")
      most_infected_countries <- head(most_infected_countries[order(most_infected_countries$today, decreasing = T),],11)
      countriesOI <- most_infected_countries$Country.Region
      meltdf_most_infected <- meltdf[meltdf$Country.Region %in% countriesOI,]
      # plot time series graph for countries of interest of cumulative deaths
      ggplot(meltdf_most_infected,
             aes(x=factor(variable, levels = unique(variable)),
                 y=value,
                 colour=Country.Region,
                 group=Country.Region)
      ) + geom_line() +
        ggtitle("COVID-19 death prognosis in most infected countries") +
        xlab("Date-15") + ylab("Cumulative Deaths")
      
      
    }
  )
  
}