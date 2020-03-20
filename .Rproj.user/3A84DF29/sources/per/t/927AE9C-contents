getwd()

## load libraries
library(ggplot2)


## update repository from github


## load data

confirmed <- read.csv("C:/Users/jjehn/Documents/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
head(confirmed)

deaths <- read.csv("C:/Users/jjehn/Documents/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
head(deaths)

recovered <- read.csv("C:/Users/jjehn/Documents/COVID-19/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")
head(recovered)



## calculate estimate of infected people up to date

# set estimation coefficients
est_death_rate <- 0.01
est_doubling_time <- 5
# get column number of today's data
latest <- ncol(deaths)
# reduce deaths to country and death numbers of today day
deaths_data <- deaths[,c(2,latest)]
# rename columns
colnames(deaths_data) <- c("Country.Region", "deaths_today")
# aggregate regional death numbers countrywise
deaths_data <- aggregate(. ~ Country.Region, deaths_data, sum)
# estimate number of infections 14.3 days ago based on deaths of today
deaths_data$infected_14.3D_bd <- deaths_data$deaths_today*(1/est_death_rate)
# estimate number of actual infections based on doubling factor added on 14.3 days before infections
deaths_data$infected_today <- deaths_data$infected_14.3D_bd*2^(14.3/est_doubling_time)

head(deaths_data)
deaths_data[grep("Germany", deaths_data$Country.Region),]
deaths_data[grep("Switzerland", deaths_data$Country.Region),]
deaths_data[grep("Italy", deaths_data$Country.Region),]
deaths_data[grep("US", deaths_data$Country.Region),]
