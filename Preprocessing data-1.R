install.packages("tidyverse")
install.packages("janitor")
install.packages("lubridate")
install.packages("scales")
install.packages("ggplot2")
install.packages("xtable")

library(tidyverse)
library(janitor)
library(lubridate)
library(scales)
library(ggplot2)
library(xtable)

df1 <- read.csv("202004-divvy-tripdata.csv.csv")
df2 <- read.csv("202005-divvy-tripdata.csv.csv")
df3 <- read.csv("202006-divvy-tripdata.csv.csv")
df4 <- read.csv("202008-divvy-tripdata.csv.csv")
df5 <- read.csv("202009-divvy-tripdata.csv.csv")
df6 <- read.csv("202010-divvy-tripdata.csv")
df7 <- read.csv("202011-divvy-tripdata.csv")
df8 <- read.csv("202012-divvy-tripdata.csv")
df9 <- read.csv("202101-divvy-tripdata.csv")
df10 <- read.csv("202102-divvy-tripdata.csv")
df11 <- read.csv("202103-divvy-tripdata.csv")
df12 <- read.csv("202104-divvy-tripdata.csv")

#combining all dataframes in to the single data frame

bike_rides <- rbind(df1,df2,df3,df4,df5,df6,df7,df8,df9,df10,df11,df12)

View(bike_rides)

##Cleaning the data
#Removing unwanted fields
bike_rides <- janitor::remove_empty(bike_rides,which = c("cols"))
bike_rides <- janitor::remove_empty(bike_rides,which = c("rows"))
dim(bike_rides)

#Getting the some useful information
bike_rides <- bike_rides %>%
  select(started_at,ended_at,start_station_name,
         member_casual,rideable_type) %>%
  na.omit(start_station_name)

#Date time

bike_rides$started_at <- lubridate::ymd_hms(bike_rides$started_at)

bike_rides$ended_at <- lubridate::ymd_hms(bike_rides$ended_at)

bike_rides <- bike_rides %>% filter(start_station_name !="")
glimpse(bike_rides)
# confining to the hours 
bike_rides$start_hour <- lubridate::hour(bike_rides$started_at)
bike_rides$end_hour <- lubridate::hour(bike_rides$ended_at)

bike_rides$Minutes <- difftime(bike_rides$ended_at,bike_rides$started_at,units = c("mins"))
bike_rides$hours <- difftime(bike_rides$started_at,bike_rides$ended_at,units = c("hours"))

bike_rides<- bike_rides %>% filter(Minutes >0)
#filtering to dates
bike_rides$start_date <- as.Date(bike_rides$started_at)
bike_rides$end_date <- as.Date(bike_rides$ended_at)

bike_rides %>% count(start_hour, sort = T) %>% 
  ggplot()+ geom_line (aes(x=start_hour,y=n))+scale_y_continuous(labels = comma)+
  labs(title = "Number of bike rides per hour : For previous year",x="Start hour",y="No.of Rides")





df <- bike_rides %>% filter(hours>0) %>% drop_na()%>% select(-duration2)
View(df)

##Making the dataframe of Summary

bike_rides2 <- bike_rides %>% group_by(weekly= floor_date(Ymd, "week"),start_hour) %>%
  summarise(
    Minutes = sum(Minutes),
    Mean = mean(Minutes),
    median = median(Minutes),
    Max = max(Minutes),
    Min = min(Minutes),
    Count = n()
  ) %>% ungroup()
# Data summary for Hour count
summary(bike_rides2$Count)

#creating a table for hourly data
xtabs(bike_rides2$count~bike_rides2$start_hour)
bike_rides2$cntMA <- forecast::ma(bike_rides2$Count,30)

bike_rides2$Monthly <-lubridate::month(bike_rides2$Weekly)

bike_rides2 %>%
  ggplot()+ geom_col (aes(x=weekly,y=count))+scale_y_continuous(labels = comma)+
  labs(title = "Number of bike rides per day",subtitle = "(based on 28days moving average",y="Average rides per day")+
  facet_wrap(~month,scale="free") + theme(axis.text.x = element_text(angle = 45))

