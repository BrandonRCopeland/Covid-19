library(RCurl)
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

options(width = 300)

setwd("C:/Users/brcopela/OneDrive")

confirmed <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
deaths <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered <- getURL("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

df.confirmed <- read.csv(text = confirmed, stringsAsFactors = FALSE)
df.dead <- read.csv(text = deaths, stringsAsFactors = FALSE)
df.recovered <- read.csv(text = recovered, stringsAsFactors = FALSE)

statesAbbreviations <- c('AL',
                         'AK',
                         'AZ',
                         'AR',
                         'CA',
                         'CO',
                         'CT',
                         'DE',
                         'FL',
                         'GA',
                         'HI',
                         'ID',
                         'IL',
                         'IN',
                         'IA',
                         'KS',
                         'KY',
                         'LA',
                         'ME',
                         'MD',
                         'MA',
                         'MI',
                         'MN',
                         'MS',
                         'MO',
                         'MT',
                         'NE',
                         'NV',
                         'NH',
                         'NJ',
                         'NM',
                         'NY',
                         'NC',
                         'ND',
                         'OH',
                         'OK',
                         'OR',
                         'PA',
                         'RI',
                         'SC',
                         'SD',
                         'TN',
                         'TX',
                         'UT',
                         'VT',
                         'VA',
                         'WA',
                         'WV',
                         'WI',
                         'WY')
statesNames <- c('Alabama',
                 'Alaska',
                 'Arizona',
                 'Arkansas',
                 'California',
                 'Colorado',
                 'Connecticut',
                 'Delaware',
                 'Florida',
                 'Georgia',
                 'Hawaii',
                 'Idaho',
                 'Illinois',
                 'Indiana',
                 'Iowa',
                 'Kansas',
                 'Kentucky',
                 'Louisiana',
                 'Maine',
                 'Maryland',
                 'Massachusetts',
                 'Michigan',
                 'Minnesota',
                 'Mississippi',
                 'Missouri',
                 'Montana',
                 'Nebraska',
                 'Nevada',
                 'New Hampshire',
                 'New Jersey',
                 'New Mexico',
                 'New York',
                 'North Carolina',
                 'North Dakota',
                 'Ohio',
                 'Oklahoma',
                 'Oregon',
                 'Pennsylvania',
                 'Rhode Island',
                 'South Carolina',
                 'South Dakota',
                 'Tennessee',
                 'Texas',
                 'Utah',
                 'Vermont',
                 'Virginia',
                 'Washington',
                 'West Virginia',
                 'Wisconsin',
                 'Wyoming')

df.states <- data.frame(statesAbbreviations, statesNames, stringsAsFactors = FALSE)

lastColConfirmed <- tail(df.confirmed %>% tbl_vars(), n=1)
lastColDead <- tail(df.dead %>% tbl_vars(), n=1)
lastColRecovered <- tail(df.recovered %>% tbl_vars(), n=1)

df.confirmed <- df.confirmed %>% 
  gather(Date, Cases, X1.22.20:lastColConfirmed) %>%
  mutate(Date = as.Date(substr(Date, 2, length(Date)), format = "%m.%d.%y"),
         Type = "Confirmed",
         State = ifelse(substr(str_sub(Province.State,-4,-1),1,1) == ",", str_sub(Province.State,-2,-1), substr(Province.State, 1, length(Province.State)))) %>%
  left_join(df.states, by = c("State" = "statesAbbreviations")) %>%
  mutate(Province.State = ifelse(is.na(statesNames), State, statesNames)) %>%
  group_by(Country.Region, Province.State, Date, Type) %>%
  summarise(Cases = sum(Cases))

df.dead <- df.dead %>% 
  gather(Date, Cases, X1.22.20:lastColDead) %>%
  mutate(Date = as.Date(substr(Date, 2, length(Date)), format = "%m.%d.%y"),
         Type = "Dead",
         State = ifelse(substr(str_sub(Province.State,-4,-1),1,1) == ",", str_sub(Province.State,-2,-1), substr(Province.State, 1, length(Province.State)))) %>%
  left_join(df.states, by = c("State" = "statesAbbreviations")) %>%
  mutate(Province.State = ifelse(is.na(statesNames), State, statesNames)) %>%
  group_by(Country.Region, Province.State, Date, Type) %>%
  summarise(Cases = sum(Cases))

df.recovered <- df.recovered %>% 
  gather(Date, Cases, X1.22.20:lastColRecovered) %>%
  mutate(Date = as.Date(substr(Date, 2, length(Date)), format = "%m.%d.%y"),
         Type = "Recovered",
         State = ifelse(substr(str_sub(Province.State,-4,-1),1,1) == ",", str_sub(Province.State,-2,-1), substr(Province.State, 1, length(Province.State)))) %>%
  left_join(df.states, by = c("State" = "statesAbbreviations")) %>%
  mutate(Province.State = ifelse(is.na(statesNames), State, statesNames)) %>%
  group_by(Country.Region, Province.State, Date, Type) %>%
  summarise(Cases = sum(Cases))

df <- df.confirmed %>%
  left_join(df.dead, by = c("Province.State", "Country.Region", "Date"), suffix = c("", "_dead")) %>%
  left_join(df.recovered, by = c("Province.State", "Country.Region", "Date"), suffix = c("", "_recovered")) %>%
  select(Province.State, Country.Region, Date, Cases, Cases_dead, Cases_recovered) %>%
  rename(Dead = Cases_dead,
         Recovered = Cases_recovered) %>%
  group_by(Province.State, Country.Region, Date) %>%
  summarise(Cases = sum(Cases),
            Dead = sum(Dead),
            Recovered = sum(Recovered)) %>% 
  group_by(Country.Region, Province.State) %>%
  mutate(New_Cases = Cases - replace_na(lag(Cases), 0),
         New_Deaths = Dead - replace_na(lag(Dead), 0)) %>%
  ungroup() %>%
  select(Country.Region, Province.State, Date, Cases, Dead, Recovered, New_Cases, New_Deaths)

write.csv(df, "covid-19-data.csv")
