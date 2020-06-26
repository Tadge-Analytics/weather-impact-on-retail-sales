library(tidyverse)

# http://www.bom.gov.au/climate/data/index.shtml?bookmark=201
# http://www.bom.gov.au/climate/dwo/IDCJDW3050.latest.shtml

# an eg csv file
# "http://www.bom.gov.au/climate/dwo/202006/text/IDCJDW3050.202006.csv"


csv_url_template <- "http://www.bom.gov.au/climate/dwo/{ym_val}/text/IDCJDW3050.{ym_val}.csv"

weather_import <- seq.Date(today() - months(13), today(), by = "month") %>% 
  tibble(month = .) %>% 
  mutate(ym_val = paste0(year(month), str_pad(month(month), width = 2, pad = "0")),
         url_string = glue::glue(csv_url_template),
         data = map(url_string, ~read_lines(.x) %>% 
                      as_tibble() %>% 
                      slice(-c(1:8)) %>% 
                      separate(value, into = as.character(1:22), sep = ",") %>% 
                      select(-1) %>% 
                      set_names(
                        .[1,] %>% 
                          unlist() %>% 
                          str_squish() %>%
                          str_replace_all("[[:punct:]]", "")) %>%
                      slice(-1) %>% 
                      janitor::clean_names())) %>% 
  unnest(data) %>% 
  select(date, minimum_temperature_u_fffd_c, maximum_temperature_u_fffd_c, rainfall_mm, sunshine_hours, speed_of_maximum_wind_gust_kmh) %>% 
  mutate(date = lubridate::ymd(date),
         across(minimum_temperature_u_fffd_c:speed_of_maximum_wind_gust_kmh, ~as.numeric(.x)))


weather_import %>% 
  write_rds("weather.rds")



