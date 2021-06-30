library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(rvest)


tod <-   format(today(), format = "%Y-%m-%d")
tom <-   format(today() + 1, format = "%Y-%m-%d")

base_url <- "https://www.metoffice.gov.uk/weather/forecast/gcwvp7zue#?date="

tom_forecast <- read_html(paste0(base_url, tom))
tod_forecast <- read_html(paste0(base_url, tod))

time_scraped <- now(tzone = "Europe/London")

hour_scraped <- paste0(hour(time_scraped), ":00")

weather_type_tom <- tom_forecast %>% html_elements(".print-wide .step-symbol .icon") %>% 
  html_attr(name = "title") %>% 
  c("weather", .)



forecast_tom <- tom_forecast %>% html_elements(xpath = paste0('//*[@id="',tom,'"]/table')) %>% 
  html_table() %>% 
  .[[1]] %>% 
  slice(c(3,5,7,9,11,13,15,17)) %>% 
  rbind(weather_type_tom) %>% 
  pivot_longer(-1) %>%
  pivot_wider(names_from = 1, values_from = value) %>% 
  `colnames<-` (c("time", 
                  "fcst_precip",
                  "fcst_temp",
                  "fcst_feels_like",
                  "fcst_wind",
                  "fcst_gust",
                  "fcst_visibility", 
                  "fcst_humidity", 
                  "fcst_UV", 
                  "fcst_weather")) %>% 
  mutate(forecast_at = time_scraped, .before = time,
         forecast_for = ymd_hm(paste(tom, time))) %>% 
  select(-time)


weather_type_tod <- tod_forecast %>% html_elements(".first-day .step-symbol .icon") %>% 
  html_attr(name = "title") %>% 
  c("weather", .)


                           

forecast_tod <- tod_forecast %>% html_elements(xpath = paste0('//*[@id="',tod,'"]/table')) %>% 
  html_table() %>% 
  .[[1]] %>% 
  slice(c(3,5,7,9,11,13,15,17)) %>% 
  rbind(weather_type_tod) %>% 
  pivot_longer(-1) %>%
  pivot_wider(names_from = 1, values_from = value) %>% 
  `colnames<-` (c("time", 
                  "fcst_precip",
                  "fcst_temp",
                  "fcst_feels_like",
                  "fcst_wind",
                  "fcst_gust",
                  "fcst_visibility", 
                  "fcst_humidity", 
                  "fcst_UV", 
                  "fcst_weather")) %>% 
  mutate(time = ifelse(time == "Now", hour_scraped, time),
         forecast_at = time_scraped, .before = time,
         forecast_for = ymd_hm(paste(tod, time))) %>% 
  select(-time)

forecast <- bind_rows(forecast_tod, forecast_tom)


#write_csv(forecast,  "data/all_forecasts.csv")

all_forecasts <- read_csv("data/all_forecasts.csv",  col_types = "TTccccccccc")

all_forecasts <- bind_rows(all_forecasts, forecast) %>% distinct()

write_csv(all_forecasts,  "data/all_forecasts.csv")

 
 
