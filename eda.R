#EDA
setwd("~/Documents/Studies/Big_Data/thrd_proj")

library(tidyverse)
library(fpp3)

# loading the data
data1 = read_csv('price_data_20220604.csv')
data2 = read_csv('price_data_20220711.csv')
data3 = read_csv('price_data_20220809.csv')


# arranging and cleaning the data

# make the data by minutes because the change in prices is only after one minutes

ts1 =  data1 %>% 
  mutate(
    minute = minute(time),
    hour = hour(time),
    day = day(time),
    month = month(time),
    year = year(time))%>% 
  select(-c(time)) %>% 
  distinct() %>% 
  mutate(time = make_datetime( year, month, day, hour, minute))


ts2 =  data2 %>% 
  mutate(
    minute = minute(time),
    hour = hour(time),
    day = day(time),
    month = month(time),
    year = year(time))%>% 
  select(-c(time)) %>% 
  distinct() %>% 
  mutate(time = make_datetime( year, month, day, hour, minute))



ts3 =  data3 %>% 
  mutate(
    minute = minute(time),
    hour = hour(time),
    day = day(time),
    month = month(time),
    year = year(time))%>% 
  select(-c(time)) %>% 
  distinct() %>% 
  mutate(time = make_datetime( year, month, day, hour, minute))

# appending: rows of tf1 and then tf2 & tf3
full_ts = ts1 %>% 
  bind_rows(ts2) %>% 
  bind_rows(ts3) %>% 
  #  unselect the year because it's a constant
  select(-year) %>% 
 # make the day as the date
  mutate( day_of_week = wday(time),
          date = date(time))

# special occasion, bidens visit, closing of hof roads, shavuot
# bidens visit 13-15 july
# hof 4.8 22:00 , 5.9 12:00
special_event = c('2022-06-05','2022-07-13','2022-07-14','2022-07-15', '2022-08-04','2022-08-05')

# add the special event indicator, and also evening indicator
full_ts = full_ts %>% 
  mutate(special_day = ifelse(as.character(date) %in% special_event, 1, 0),
         late = ifelse(hour >= 13, 1, 0) )
full_ts %>% glimpse()

##


# 4 graphs
# daily vallues
# aggregating daily avg price by early and late drive before or after 12 pm
daily_avg = full_ts %>% 
  group_by(date, late) %>% 
  summarize(mean_price = mean(price), max_price = max(price))
  
p1 = ggplot(daily_avg, mapping = aes(x = date, y = mean_price))+
  geom_line()+
  xlab("")

p1 +
  facet_wrap(vars(late))
## same but with max 

p2 = ggplot(daily_avg, mapping = aes(x = date, y = max_price))+
  geom_line()+
  xlab("")

p2 +
  facet_wrap(vars(late))


# check that bump in late and then only focus on the early times
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")



