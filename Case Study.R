## to get all of the packages loaded up

library(tidyverse)
library(lubridate)
library(rmarkdown)
library(ggplot2)
library(janitor)
library(chron)
library(scales)

## to set the present directory to the case study folder

setwd("/Users/vladbularca/Desktop/Case_Study")

# importing the 12 months of data and making them as dataframes 

sept20 <- read.csv("SEPT_20.csv")
oct20 <- read.csv("OCT_20.csv")
nov20 <- read.csv("NOV_20.csv")
dec20 <- read.csv("DEC_20.csv")
jan21 <- read.csv("JAN_21.csv")
feb21 <- read.csv("FEB_21.csv")
mar21 <- read.csv("MAR_21.csv")
apr21 <- read.csv("APR_21.csv")
may21 <- read.csv("MAY_21.csv")
june21 <- read.csv("JUNE_21.csv")
july21 <- read.csv("JULY_21.csv")
aug21 <- read.csv("AUG_21.csv")

#changing column type from integer to character so that bind_rows doesn't produce 
# an error, 

sept20$start_station_id <-as.character(sept20$start_station_id)
sept20$end_station_id <-as.character(sept20$start_station_id)

oct20$start_station_id <-as.character(oct20$start_station_id)
oct20$end_station_id <-as.character(oct20$start_station_id)

nov20$start_station_id <-as.character(nov20$start_station_id)
nov20$end_station_id <-as.character(nov20$start_station_id)

# for some reason in some of the 12 monthly files, the start station and end 
# station were stored as integers, but some station names include letters. 

# combining all of the data into one table

year_data <- bind_rows(sept20, oct20, nov20, dec20, jan21, 
feb21, mar21, apr21, may21, june21, july21, aug21)

# how many rows in the new table 
nrow(year_data)

# get the date from start times into a date format in a new column, 
# before it had the date and time the ride started at 

year_data$date <- as.Date(year_data$started_at)

# Also want to split up the new data column into day, year  and so you 
# an easily access different information from the start time and use it to 
# analyze the data and difference in use of service between members and non
# non members 

year_data$day <- format(as.Date(year_data$date), "%d")
year_data$month <- format(as.Date(year_data$date), "%m")
year_data$year <- format(as.Date(year_data$date), "%Y")

# adding a column that has the start hour and end hour for each ride
year_data$start_hour <- lubridate::hour(year_data$started_at)
year_data$end_hour <- lubridate::hour(year_data$ended_at)

# converting the ride_length to a different a numeric value, so that
# I can do calculations on it, It used to be a character 

year_data$ride_length <- chron(times=year_data$ride_length)

is.numeric(year_data$ride_length)

# Dropping any rows that have ride_length less than 0 (already did this in
# excel), and then taking out all of the test rides "HQ QR" and making it into
# a new clean data table

year_data2 <- filter(year_data, year_data$start_station_name 
                     != "Base - 2132 W Hubbard Warehouse")

year_data2 %>% count(member_casual)

# At this point all of the data is prepared and cleaned and formatted now 
# it can be used to analyze and find some trends 

mean(year_data2$ride_length)
max(year_data2$ride_length)
min(year_data2$ride_length)
median(year_data2$ride_length)


# calculate some summary values based on the member type to see the differences 

aggregate(year_data2$ride_length ~ year_data2$member_casual, FUN = mean)
aggregate(year_data2$ride_length ~ year_data2$member_casual, FUN = median)
aggregate(year_data2$ride_length ~ year_data2$member_casual, FUN = max)
aggregate(year_data2$ride_length ~ year_data2$member_casual, FUN = min)

year_data2 %>% count(member_casual, sort = TRUE)

year_data2 %>% count(month, sort = TRUE) 

# average ride length is more than double for members, so they are probably
# riding them less consistently but for longer 

# finding average ride length based on day of the week and days of the week
# (1 = monday 7 = sunday)
aggregate(year_data2$ride_length ~ year_data2$member_casual
          + year_data2$day_of_week, FUN = mean)


# I sorted by the hour that each ride begins at and counted the number of rides

year_data2 %>% count(start_hour, sort = TRUE) 

year_data2 %>% count(start_hour, sort = TRUE) %>% 
  ggplot() +geom_line(aes(x=start_hour, y=n)) +
  scale_y_continuous(labels=comma) + 
  labs(title ="star hour the total rides share datafrom bike",y=
    "number of rides") 

# this is based on ridership 

year_data2 %>% group_by(member_casual) %>% count(start_hour) %>% 
  arrange(member_casual, -n)

#Then I graphed this to see the difference 

year_data2 %>% group_by(member_casual) %>% count(start_hour) %>% 
  arrange(member_casual, -n) %>% ggplot() +
  geom_line(aes(x=start_hour, y=n, colour=member_casual, group=member_casual)) +
  scale_y_continuous(labels=comma)

# summary table of the amount of rides based on the day of the week and average
# length of each ride for both members and casuals to get a comparison

year_data2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
    group_by(member_casual, weekday) %>% 
    summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
    arrange(member_casual, weekday) 

# graph of above table to see the differences in rides between the two 

year_data2 %>% mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n(),average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday) %>%
  ggplot(aes(x = weekday, y = number_of_rides, fill= member_casual)) + 
  geom_col(position = "dodge") + scale_y_continuous(labels=comma)


# have to do one for most popular stations based on ridership and do a 
# total one and then one for each, at the end of the day, so I'm comparing 
# the two and then seeing how you can target more casual members 

year_data2 %>% count(start_station_name, sort = TRUE) %>% slice(1:10)

year_data2 %>% group_by(member_casual) %>% count(start_station_name) %>% 
  arrange(member_casual, -n) %>% slice(1:10) 

year_data2 %>% group_by(member_casual) %>% count(start_station_name) %>% 
  arrange(member_casual, -n) %>% slice(1:10) %>%  

# also looking at bike-type as well

year_data2 %>% count(rideable_type)

year_data2 %>% group_by(member_casual) %>% count(rideable_type) %>% 
  arrange(member_casual, -n) 

year_data2 %>% group_by(member_casual) %>% count(rideable_type) %>% 
  arrange(member_casual, -n)  %>% ggplot(aes(x=rideable_type, y = n, 
  fill=member_casual)) + geom_col(position="dodge") + 
  scale_y_continuous(labels=comma)

# not too sure classic vs docked bike is? 

# I am converting year_data2 to csv so it can be exported and
# analyzed and visualized using tableau going forward

write.csv(year_data2, file="/Users/vladbularca/Desktop")




