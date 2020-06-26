
library(tidyverse)
library(lubridate)
library(broom)

theme_set(theme_minimal())

weather_import <- read_rds("weather.rds") %>% 
  mutate(rainfall = if_else(rainfall_mm == 0, "no rain", "rain")) %>% 
  select(date, rainfall)

random_daily_sales <- read_rds("random_sales.rds") %>% 
  mutate(day_index = row_number(),
         weekday = wday(date, label = TRUE, week_start = 1),
         weekday = factor(weekday, ordered = FALSE)) %>% 
    left_join(weather_import, by = c("date" = "date")) 





random_daily_sales %>% 
  ggplot() +
  aes(date, total_sales) +
  geom_point(aes(col = rainfall)) +
  geom_smooth(method = "lm", se = FALSE) + 
  expand_limits(y = 0)


random_daily_sales %>% 
  lm(total_sales ~ day_index, data = .) %>% 
  tidy()

0.0539*365



random_daily_sales %>% 
  ggplot() +
  aes(total_sales, fill = rainfall) +
  geom_density(alpha = 0.6)







random_daily_sales %>% 
  ggplot() +
  aes(rainfall, total_sales, col = rainfall, fill = rainfall) +
  geom_point(stat = "identity", alpha = 0.5, position = position_jitter(0.1)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.5, show.legend = F) +
  stat_summary(fun = mean, aes(label = "mean"), hjust = 1.7, vjust = -0.5, geom = "text", show.legend = F) +
  stat_summary(fun = mean, aes(label = round(after_stat(y), 1)), hjust = -1, vjust = -0.5, geom = "text", show.legend = F)



random_daily_sales %>% 
  lm(total_sales ~ day_index + rainfall, data = .) %>% 
  tidy()


# random_daily_sales %>% 
#   lm(total_sales ~ -1 + rainfall, data = .) %>% 
#   tidy()



random_daily_sales %>% 
  ggplot() +
  aes(rainfall, total_sales, col = rainfall) +
  geom_point(stat = "identity", alpha = 0.5) +
  facet_grid(. ~ weekday) +
  scale_x_discrete(labels = element_blank()) +
  theme_grey()




random_daily_sales %>%
  lm(total_sales ~ day_index + rainfall + weekday, data = .) %>% 
  tidy() %>% 
  filter(p.value <= 0.05)




random_daily_sales %>%
  lm(total_sales ~ day_index + rainfall + weekday + rainfall*weekday, data = .) %>% 
  tidy() %>% 
  filter(p.value <= 0.05)








# this tells us..?
# compared to monday... Wed, THur and Sat are statistically signifiacantly different.
# there are stil differences in the mean between Monday wih Tue and Fri... 
# but it is not big enough for us to say for sure that the days' rates were any different.
# ie, may as well lump these all in together, for now... give them on big average
# is this right?

# adding in the day_index term (Which is significant) allows us to see that 
# there is certainly a downward trend over the current, whole period of data
# that, on average, for every 3 days from the start... there is one less sale
# or for each month, there are ~10 less sales, per day













# improve this graph so the bars are thicker than 1 pixel!
# might be good to have sidesways distributions to these -of 2 sd? 
# that would be realy cool


random_daily_sales %>%
  group_by(weekday) %>% 
  summarise(sum_sales = sum(total_sales),
            count_sales = n(),
            average_sales = sum_sales/count_sales) %>% 
  mutate(overall_avg = sum(sum_sales)/sum(count_sales),
         diff_from_avg = average_sales - overall_avg) %>% 
  ggplot() +
  aes(x = weekday, xend = weekday, y = overall_avg, yend = average_sales) +
  geom_segment()


