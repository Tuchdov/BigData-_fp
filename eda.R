#EDA
setwd("~/Documents/Studies/Big_Data/BigData_fp/")

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
special_event = c('2022-06-05','2022-07-13','2022-07-14','2022-07-15',
                  '2022-08-04','2022-08-05')

# add the special event indicator, and also evening indicator
full_ts = full_ts %>% 
  mutate(special_day = ifelse(as.character(date) %in% special_event, 1, 0),
         late = ifelse(hour >= 12, 'after 12 am', 'before 12 am') )

data1 %>% 
  filter(price != 7)
#### End of Data preperation ####
full_ts %>% glimpse()

## Making Graphs ##


# 4 graphs
# daily vallues
# aggregating daily avg price by early and late drive before or after 12 pm
date_avg = full_ts %>% 
  group_by(date, late, day_of_week) %>% 
  summarize(mean_price = mean(price), max_price = max(price))

p1 = ggplot(date_avg, mapping = aes(x = date, y = mean_price))+
  geom_line()+
  xlab("")

date_avg
p1 +
  facet_wrap(vars(late))
## same but with max 

p2 = ggplot(date_avg
            , mapping = aes(x = date, y = max_price, label =day_of_week ))+
  geom_label(aes(label=day_of_week),label.padding = unit(0.1, "lines"),
             label.size = 0.05, nudge_x = 0.2)+
  xlab("")+
  ggtitle('max priceall month')

late_labs <- c("Orange Juice", "Vitamin C")
names(date_avg$late) <- c("OJ", "VC")

p2
p2 +
  facet_wrap(vars(late), labeller = labeller(late_labs))



p2 = ggplot(date_avg
            , mapping = aes(x = date, y = max_price ,
                            color = factor(day_of_week)))+
  geom_point(stat = 'identity')+
  xlab("")+
  ylab('highest price')+
  ggtitle('max priceall month')
p2 +
  facet_wrap(vars(late))+
  guides(color=guide_legend(title="Day"))

# getting the pecuiliar date point
date_avg %>% 
  filter(late == 1 & max_price != 7) 

day_avg = full_ts %>% 
  group_by(day_of_week, late) %>% 
  summarize(mean_price = mean(price), max_price = max(price))

day_avg 

full_ts %>% 
  group_by(date,day_of_week) %>%
  filter(late == 'before 12 am') %>% 
  summarise(mean_price = mean(price)) %>% 
  ggplot(mapping = aes(x = factor(day_of_week), y = mean_price, group = day_of_week,
                       fill = factor(day_of_week)
  ))+
  geom_boxplot()+
  stat_summary(fun.y=mean, geom="point", shape=23, size=4)+
  ylab("average price")+
  xlab("")+
  ggtitle("Early day average prices")+
  theme_classic()+
  guides(fill =guide_legend(title="Day"))



# during the week I want hourly prices

full_ts %>% 
  group_by(date,day_of_week, hour) %>%
  filter(late == "after 12 am" & !(day_of_week %in% c(6,7)) & hour >= 6 ) %>% 
  ggplot(mapping = aes(x = factor(hour), y = price, group = hour
  ))+
  geom_boxplot()+
  ggtitle("avg price by early day")+
  facet_wrap(~day_of_week)

# for competition after 11 all prices should be 7.

full_ts %>% 
  group_by(date,day_of_week) %>%
  filter(late == 0) %>% 
  summarise(max_price = max(price)) %>% 
  ggplot(mapping = aes(x = factor(day_of_week), y = max_price, group = day_of_week))+
  geom_boxplot()


p3 = ggplot(day_avg, mapping = aes(x = day_of_week, y = mean_price))+
  geom_line()+
  xlab("")

p3+
  facet_wrap(vars(late))+
  ggtitle('avg daily price acrros three months')

# early week data
early_ts = full_ts %>% 
  # not late not a weekend and after 6 am
  filter(late == "before 12 am"  & !(day_of_week %in% c(6,7)) & hour >= 6 )

# line plot of eaely times and prices

q2 = early_ts %>% 
  mutate(tic = hms::hms(NULL, minute, hour),
         # moving avg
         ma5 = slider::slide_dbl(price, mean,.before = 2, .after = 2),
         # moving avg of moving avg
         ma5x2 = slider::slide_dbl(ma5, mean,
                                   .before = 1, .after = 0))


q2 %>% 
  group_by(tic, day_of_week) %>% 
  summarise(avg_price = mean(price), avg_moving_prc = mean(ma5x2) ) %>% 
  ggplot(mapping = aes(x = tic, y = avg_price, color = factor(day_of_week))) +
  geom_line() +
  labs(y = "toll price",
       x = "time",
       title = "moving average of toll prices by working days")+
  scale_x_time()+
  facet_grid(vars(day_of_week))+
  theme_bw()+
  guides(color =guide_legend(title="Day"))


# check that bump in late and then only focus on the early times

