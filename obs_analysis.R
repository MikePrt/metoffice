library(tidyverse)
library(lubridate)
library(rvest)

## All times in UTC

all_obs <- read_csv("data/all_obs.csv",  col_types = "Tcccccccc", na = c("", "NA", "-"))

range(all_obs$obs_at)


all_forecasts <- read_csv("data/all_forecasts.csv",  col_types = "TTccccccccc") %>% 
  mutate(time_diff = round(difftime( forecast_for, forecast_at,  units = "hours")), .after = forecast_for)

range(all_forecasts$forecast_for)

## Analysis starts here

all_obs <- all_obs %>% mutate(temp = parse_number(temp),
                              rain = weather %in% c("Drizzle", "Heavy rain" , "Light rain"))

all_obs %>% summarise(pct_rain = mean(rain))

all_obs %>% 
  ggplot(aes(x = obs_at, y = temp)) + geom_line()


all_obs %>%  count(weather) %>% pull(weather)

weather_lookup <- tibble(weather = c("Cloudy" ,"Drizzle", "Fog" , "Heavy rain" , "Light rain" , "Mist"  ,         
                                      "Overcast" ,  "Sunny day" , "Sunny intervals"),
                         code = c(7,3,4,1,2,5,6, 9,8)) %>% arrange(code)

all_obs %>% select(obs_at, weather) %>% left_join(weather_lookup) %>% 
  ggplot(aes(x= obs_at, y = code)) + geom_point() + geom_line()




## join forecast with observations

# time issues should be sorted from about midday on 1/7/21 

combined_forecast_obs <- inner_join(all_forecasts, all_obs, by = c("forecast_for" = "obs_at" ) ) %>% 
  filter(forecast_at >= ymd_hms("2021-07-01 12:00:00")) %>% 
  select(forecast_at, forecast_for, time_diff, fcst_weather, weather ) %>% 
  mutate(accurate = (fcst_weather == weather)) %>% 
  arrange(forecast_for)

combined_forecast_obs %>% summarise(pct_accurate = mean(accurate))





