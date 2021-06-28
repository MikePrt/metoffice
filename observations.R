library(tidyverse)
library(lubridate)
library(rvest)

# Get current observations - last 24 hours


obs_url <- "https://www.metoffice.gov.uk/weather/observations/gcwvp7zue"

current_obs <- read_html(obs_url)

#current_obs <- read_html("C:/Users/Mike/Dropbox/Zapier/observations2021-06-28T050029+0100.html")




yesterday_obs <- current_obs %>% 
  html_elements("#day0 > table") %>% 
  html_table() %>% .[[1]] %>% 
  rename(measurement = "") %>% 
  slice(c(4,6, 8,10,12,14,16)) %>% 
  pivot_longer(-1) %>%
  pivot_wider(names_from = 1, values_from = value) %>% 
  mutate(date = today() - 1, .before = name)

today_obs <- current_obs %>% 
  html_elements("#day1 > table") %>% 
  html_table() %>% .[[1]] %>% 
  rename(measurement = "") %>% 
  slice(c(4,6, 8,10,12,14,16)) %>% 
  pivot_longer(-1) %>%
  pivot_wider(names_from = 1, values_from = value) %>% 
  mutate(date = today(), .before = name)

weather_obs <- current_obs %>% html_elements(".step-symbol .icon") %>% 
  html_attr(name = "title")

recent_obs <- bind_rows(yesterday_obs, today_obs) %>% 
  mutate(weather = weather_obs) %>% 
  rename(time = name,
         temp = `Temperature in degrees Celsius`,
         wind = `Wind direction and speed in miles per hour`,
         pressure = `Pressure in hectopascals`,
         pressure_trend = `Pressure tendency`,
         visibility = Visibility,
         humidity = Humidity) %>% 
  mutate(obs_at = ymd_hm(paste(date, time)), .before = temp, .keep = "unused") 



all_obs <- read_csv("data/all_obs.csv",  col_types = "Tcccccccc")

all_obs <- bind_rows(all_obs, recent_obs) %>% distinct()

write_csv(all_obs,  "data/all_obs.csv")
