library(httr)
library(tidyverse)
library(leaflet)
library(kableExtra)

deaths <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
View(time_series_19_covid_Deaths)


cases <- read_csv("https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_daily_reports/02-26-2020.csv")
View(cases)

### MODIFY Deaths

deaths$sum <- as.double(NA)
  
deaths <- deaths %>% mutate(sum = rowSums(.[5:38]))

deaths2 <- deaths %>% 
  rename('Province' = `Province/State`, 'Country' = 'Country/Region')


#### MODIFY Cases
cases2 <- cases %>% 
  rename('Province' = `Province/State`, 'Country' = 'Country/Region')

cases3 <- cases2 %>% 
  select(Country, Confirmed, Deaths, Recovered) %>% 
  group_by(Country) 

cases3 <- summarise(cases3, 
                    count = n(), 
                    Confirmed = sum(Confirmed),
                    Deaths = sum(Deaths),
                    Recovered = sum(Recovered)
)


cases3$rate <- ((cases3$Deaths/cases3$Confirmed) * 100)
cases3$rate <- round(cases3$rate, digits = 1)

cases3 <- arrange(cases3, Country)

###### Make some nice plots

l1 <- leaflet(data = deaths2) %>% addTiles() %>%
  addMarkers(~Long, ~Lat, popup = ~as.character(sum), label = ~as.character(Country))
l1






getColor <- function(deaths2) {
  sapply(deaths2$sum, function(sum) {
    if(sum < 1) {
      "green"
    } else if(sum <= 10) {
      "orange"
    } else {
      "red"
    } })
}


icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(deaths2)
)

l2 <- leaflet(deaths2) %>% addTiles() %>%
  addAwesomeMarkers(~Long, ~Lat, icon=icons, label=~as.character(sum))

l2

k1 <- kable(cases3, caption = "Death percentage from Novid Coronavirus-19 by Country.") %>% 
  kable_styling(bootstrap_option = c("striped", "hover", "condensed"))

k1




