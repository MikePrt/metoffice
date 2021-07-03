library(tidyverse)
library(lubridate)
library(rvest)

## All times in UTC

## Preprocessing

all_obs <- read_csv("data/all_obs.csv",  col_types = "Tcccccccc", na = c("", "NA", "-")) %>% 
  mutate(temp = parse_number(temp), rain = weather %in% c("Drizzle", "Heavy rain" , "Light rain"))

range(all_obs$obs_at)


all_forecasts <- read_csv("data/all_forecasts.csv",  col_types = "TTccccccccc") %>% 
  mutate(time_diff = round(difftime( forecast_for, forecast_at,  units = "hours")), .after = forecast_for)

range(all_forecasts$forecast_for)

## Combination of observation data with EA rainfall data

EA_rain <- read_csv("C:/Users/Mike/Documents/R/Weather/hist.csv") %>% 
  mutate(hr = ceiling_date(dateTime, unit = "hour", change_on_boundary = NULL))

EA_rain_hr <- EA_rain %>% group_by(hr) %>% summarise(rain_hr = sum(value))

obs_rain <- all_obs %>% select(obs_at, weather, rain) %>% left_join(EA_rain_hr, by = c("obs_at" = "hr" ))

## Exceptions
obs_rain %>% filter(rain == FALSE, rain_hr > 0)
obs_rain %>% filter(rain == TRUE, rain_hr == 0)


## Analysis starts here

all_obs %>% summarise(pct_rain = mean(rain))

all_obs %>% 
  ggplot(aes(x = obs_at, y = temp)) + geom_line()


all_obs %>%  count(weather) %>% pull(weather)





## join forecast with observations

# time issues should be sorted from about midday on 1/7/21 

combined_forecast_obs <- inner_join(all_forecasts, all_obs, by = c("forecast_for" = "obs_at" ) ) %>% 
  filter(forecast_at >= ymd_hms("2021-07-01 12:00:00")) %>% 
  select(forecast_at, forecast_for, time_diff, fcst_weather, weather ) %>% 
  mutate(accurate = (fcst_weather == weather)) %>% 
  arrange(forecast_for)

combined_forecast_obs %>% summarise(pct_accurate = mean(accurate))





