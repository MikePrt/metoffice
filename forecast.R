library(dplyr)
library(tidyr)
library(readr)
library(lubridate)
library(rvest)


tom <-   format(today() + 1, format = "%Y-%m-%d")

base_url <- "https://www.metoffice.gov.uk/weather/forecast/gcwvp7zue#?date="

tom_forecast <- read_html(paste0(base_url, tom))

time_scraped <- now()

weather_type <- tom_forecast %>% html_elements(".print-wide+ .print-wide .step-symbol .icon") %>% 
  html_attr(name = "title") %>% 
  c("weather", .)



forecast <- tom_forecast %>% html_elements(xpath = paste0('//*[@id="',tom,'"]/table')) %>% 
  html_table() %>% 
  .[[1]] %>% 
  slice(c(3,5,7,9,11,13,15,17)) %>% 
  rbind(weather_type) %>% 
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

write_csv(forecast,  "data/forecast.csv")



 
 
