library(tidyverse)
library(lubridate)
library(rvest)

## All times in UTC

all_obs <- read_csv("data/all_obs.csv",  col_types = "Tcccccccc", na = c("", "NA", "-"))

range(all_obs$obs_at)


all_forecasts <- read_csv("data/all_forecasts.csv",  col_types = "TTccccccccc") %>% 
  mutate(time_diff = round(difftime( forecast_for, forecast_at,  units = "hours")))

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


all_forecasts <- read_csv("data/all_forecasts.csv",  col_types = "TTccccccccc", na = c("", "NA", "-")) %>% 
  mutate(time_diff = round(difftime( forecast_for, forecast_at,  units = "hours")))


## join forecast with observations

combined_forecast_obs <- inner_join(all_forecasts, all_obs, by = c("forecast_for" = "obs_at" ) )





