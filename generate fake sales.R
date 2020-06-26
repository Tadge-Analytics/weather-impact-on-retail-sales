library(tidyverse)
library(lubridate)

weather_import <- read_rds("weather.rds") %>% 
  mutate(rainfall = if_else(rainfall_mm == 0, "no rain", "rain")) %>% 
  select(date, rainfall)


day_of_week_effects <- tibble::tribble(
  ~weekday, ~dow_diff,
  "Sun",          -30L,
  "Mon",            0L,
  "Tue",           15L,
  "Wed",           25L,
  "Thu",           35L,
  "Fri",           20L,
  "Sat",          -35L) %>% 
  mutate(dow_diff = dow_diff/100)



set.seed(123)
random_daily_sales <- seq.Date(to = floor_date(today(), "month")-1, 
                               from = floor_date(today() - months(13), "month"), 
                               by = "day") %>% 
  tibble(date = .) %>% 
  
  mutate(weekday = wday(date, label = TRUE), 
         day_index = row_number()) %>% 
  
  left_join(weather_import, by = "date") %>% 
  left_join(day_of_week_effects, by = "weekday") %>% 
  
  mutate(average_sales = 200, 
         daily_increase = runif(n(), 0.05/365, 0.25/365),
         daily_increase = daily_increase * day_index,
         
         rain_effect = if_else(rainfall == "rain", runif(n(), 0.15, 0.35), 0),
         rain_effect = if_else(weekday == "Fri", -rain_effect, rain_effect),
  
         dow_diff = map_dbl(dow_diff, ~rnorm(1, .x, 0.1)), # stdev if 10% for each day of week percentage
         
         total_sales = average_sales + daily_increase*average_sales + rain_effect*average_sales + dow_diff*average_sales,
         total_sales = round(total_sales)) %>%
  
  select(date, total_sales)


random_daily_sales %>% 
  write_rds("random_sales.rds")





