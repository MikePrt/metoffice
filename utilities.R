all_obs <- read_csv("data/all_obs.csv",  col_types = "Tcccccccc", na = c("", "NA", "-"))

all_obs <- all_obs %>% mutate(wind = str_replace(wind, "\r\n\r\n", replacement ="_"))



range(all_obs$obs_at)

all_obs %>% select(obs_at) %>% mutate(delta_hr = c(NA, diff(obs_at))) %>% 
  filter(delta_hr != 1)

recent_obs <- recent_obs %>%  mutate(tz = attr(obs_at,"tzone"), .after = obs_at)

write_csv(recent_obs,  "data/recent_obs.csv")

recent_obs_from_csv <- read_csv("data/recent_obs.csv", col_types = cols(.default = "c")) %>% 
  mutate(obs_at2 = ymd_hms(obs_at,tz = "UTC"),
         tz = attr(obs_at2,"tzone"),
         .after = obs_at)

recent_obs_from_csv2 <- read_csv("data/recent_obs.csv", col_types = "Tcccccccc")

recent_obs_from_csv2 %>% select(obs_at) %>% mutate(tz = attr(obs_at,"tzone")) %>% View()

all_obs %>% select(obs_at) %>% mutate(tz = attr(obs_at,"tzone")) %>% View()

forecast %>% select(forecast_for) %>% mutate(tz = attr(forecast_for,"tzone")) %>% View()


all_forecasts <- read_csv("data/all_forecasts.csv",  col_types = "TTccccccccc")

all_forecasts <- all_forecasts %>% mutate(fcst_wind = str_replace(fcst_wind, "\r\n\r\n", replacement ="_"))


#all_obs <- all_obs %>% mutate(obs_at = force_tz(obs_at, tzone = "Europe/London"))
