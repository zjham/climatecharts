library(tidyverse)
library(lubridate)
library(weathercan)
library(extrafont)

### Import & Clean
### -------------------------------------------------------------------------

#cities were chosen based on consistency of reported data over the time range at
#the international airport weather station. 

city_ids <- c("Victoria" = c(118, 51337),
              "Vancouver" = c(889, 51442),
              "Edmonton" = c(1865, 50149),
              "Calgary" = c(2205, 50430),
              "Toronto" = c(5097, 51459),
              "Ottawa" = c(4337, 49568),
              "Montreal" = c(5415, 51157),
              "Halifax" = c(6358, 50620),
              "Yellowknife" = c(1706, 51058))
#import from ec using weathercan, save copy of raw data to local disk. Download 
#can take a long time


climate_hourly_raw <- weather_dl(city_ids, interval = "hour")
climate_daily_raw <- weather_dl(city_ids, interval = "day")


write_csv(climate_hourly_raw, "data/climate_hourly_raw.csv")
write_csv(climate_daily_raw, "data/climate_daily_raw.csv")

climate_hourly <- climate_hourly_raw %>%
  mutate(city = case_when(str_detect(station_name, "VICTORIA") ~ "Victoria",
                          str_detect(station_name, "VANCOUVER") ~ "Vancouver",
                          str_detect(station_name, "EDMONTON") ~ "Edmonton",
                          str_detect(station_name, "CALGARY") ~ "Calgary",
                          str_detect(station_name, "TORONTO") ~ "Toronto",
                          str_detect(station_name, "OTTAWA") ~ "Ottawa",
                          str_detect(station_name, "MONTREAL") ~ "Montreal",
                          str_detect(station_name, "HALIFAX") ~ "Halifax",
                          str_detect(station_name, "YELLOWKNIFE") ~ "Yellowknife")) %>%
  select(-station_operator, -hmdx, -hmdx_flag, -pressure_flag, -rel_hum_flag, 
         -temp_dew_flag, -temp_flag, -visib_flag, -wind_chill_flag,
         -wind_spd_flag, -wind_dir_flag) %>%
  select(city, 1:24) %>%
  filter(!is.na(pressure)) %>%
  group_by(city, date, hour) %>%
  slice(1) %>%
  ungroup()

###create relational table with station info to dramatically reduce size of hourly data frame

station_info <- climate_hourly %>%
  select(city, station_name, station_id, prov, lat, lon, elev, climate_id, WMO_id, TC_id) %>%
  group_by_all() %>%
  slice(1)

climate_hourly <- climate_hourly %>%
  select(-station_name, -prov, -lat, -lon, -elev, -climate_id, -WMO_id, -TC_id)

#Clean daily data in order to incorporate variables such as precip in aggregated
#hourly database.

climate_daily <- climate_daily_raw %>%
  mutate(city = case_when(str_detect(station_name, "VICTORIA") ~ "Victoria",
                          str_detect(station_name, "VANCOUVER") ~ "Vancouver",
                          str_detect(station_name, "EDMONTON") ~ "Edmonton",
                          str_detect(station_name, "CALGARY") ~ "Calgary",
                          str_detect(station_name, "TORONTO") ~ "Toronto",
                          str_detect(station_name, "OTTAWA") ~ "Ottawa",
                          str_detect(station_name, "MONTREAL") ~ "Montreal",
                          str_detect(station_name, "HALIFAX") ~ "Halifax",
                          str_detect(station_name, "YELLOWKNIFE") ~ "Yellowknife")) %>%
  select(city, station_id, date, year, month, day, cool_deg_days, dir_max_gust, 
         heat_deg_days, max_temp, mean_temp, min_temp, snow_grnd, spd_max_gust,
         total_precip, total_rain, total_snow) %>%
  filter(!is.na(heat_deg_days), !is.na(max_temp), !is.na(min_temp)) %>%
  group_by(city, date) %>%
  slice(1) %>%
  ungroup()

climate_monthly_avg <- climate_hourly %>%
  group_by(city, year, month) %>%
  summarize_at(vars(pressure:wind_spd), .funs = c("mean", "sd", "min", "max"), na.rm = TRUE)


climate_yearly_avg <- climate_hourly %>%
  group_by(city, year) %>%
  summarize_at(vars(pressure:wind_spd), .funs = c("mean", "sd", "min", "max"), na.rm = TRUE) 

climate_monthly <- climate_monthly_avg %>%
  left_join(climate_daily %>%
              select(city, year, month, snow_grnd, total_precip, total_rain, total_snow) %>%
              group_by(city, year, month) %>%
              summarize_at(vars(snow_grnd, total_precip, total_rain, total_snow),
                           .funs = c("sum", "sd", "min", "max"), na.rm = TRUE)) %>%
  mutate(precip = total_precip_sum,
         temp = round(temp_mean, 1),
         date = ymd(paste(year, month, "15", sep = "-")))

climate_yearly <- climate_yearly_avg %>%
  left_join(climate_daily %>%
              select(city, year, month, snow_grnd, total_precip, total_rain, total_snow) %>%
              group_by(city, year) %>%
              summarize_at(vars(snow_grnd, total_precip, total_rain, total_snow),
                           .funs = c("sum", "sd", "min", "max"), na.rm = TRUE)) %>%
  mutate(precip = total_precip_sum,
         temp = round(temp_mean, 1),
         date = ymd(paste(year, "06", "15", sep = "-")))

climate_daily_avg <- climate_hourly %>%
  group_by(city, date) %>%
  summarize_at(vars(pressure:wind_spd), .funs = c("mean", "sd", "min", "max"), na.rm = TRUE)

climate_daily <- climate_daily_avg %>%
  left_join(select(climate_daily, city, date, snow_grnd,
                   total_precip, total_rain, total_snow), by = c("city", "date")) %>%
  mutate(precip = total_precip,
         temp = round(temp_mean, 1))

climate_monthly <- climate_monthly %>%
  mutate(temp = round(temp_mean, 1))

write_csv(climate_yearly, "data/climate_yearly.csv")
write_csv(climate_monthly, "data/climate_monthly.csv")
write_csv(climate_daily, "data/climate_daily.csv")
write_csv(climate_hourly, "data/climate_hourly.csv")
write_csv(station_info, "data/station_info.csv")



