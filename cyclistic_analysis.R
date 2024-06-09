#Importing libraries
library(data.table) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

#Uploading Data from past year

data_2023_05 <- fread("C:\\Users\\callu\\Documents\\Data Analytics Certificate\\202305-divvy-tripdata.csv")
data_2023_06 <- fread("C:\\Users\\callu\\Documents\\Data Analytics Certificate\\202306-divvy-tripdata.csv")
data_2023_07 <- fread("C:\\Users\\callu\\Documents\\Data Analytics Certificate\\202307-divvy-tripdata.csv")
data_2023_08 <- fread("C:\\Users\\callu\\Documents\\Data Analytics Certificate\\202308-divvy-tripdata.csv")
data_2023_09 <- fread("C:\\Users\\callu\\Documents\\Data Analytics Certificate\\202309-divvy-tripdata.csv")
data_2023_10 <- fread("C:\\Users\\callu\\Documents\\Data Analytics Certificate\\202310-divvy-tripdata.csv")
data_2023_11 <- fread("C:\\Users\\callu\\Documents\\Data Analytics Certificate\\202311-divvy-tripdata.csv")
data_2023_12 <- fread("C:\\Users\\callu\\Documents\\Data Analytics Certificate\\202312-divvy-tripdata.csv")
data_2024_01 <- fread("C:\\Users\\callu\\Documents\\Data Analytics Certificate\\202401-divvy-tripdata.csv")
data_2024_02 <- fread("C:\\Users\\callu\\Documents\\Data Analytics Certificate\\202402-divvy-tripdata.csv")
data_2024_03 <- fread("C:\\Users\\callu\\Documents\\Data Analytics Certificate\\202403-divvy-tripdata.csv")
data_2024_04 <- fread("C:\\Users\\callu\\Documents\\Data Analytics Certificate\\202404-divvy-tripdata.csv")

#Combine all the seperate months of data into one big data frame and create a copy in order to keep the original data in its original state
all_data <- bind_rows(data_2023_05,data_2023_06,data_2023_07,data_2023_08,data_2023_09,data_2023_10,data_2023_11,data_2023_12,data_2024_01,data_2024_02,data_2024_03,data_2024_04)
all_data_2 <- all_data

#Create new columns ride_length, day_of_week and month_of_year
all_data_2$ride_length <- difftime(all_data_2$ended_at, all_data_2$started_at)
all_data_2$day_of_week <- wday(all_data_2$started_at)
all_data_2$month_of_year <- month(all_data_2$started_at)

#Change the days from numbers to Strings
day_names <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
all_data_2 <- all_data_2 %>% mutate(day_of_week = factor(day_of_week, levels = 1:7, labels = day_names))

#Change the months from numbers to Strings
month_names <- c("January", "February", "March", "April", "May", "June", "July", "August","September","October","November","December")
all_data_2 <- all_data_2 %>% mutate(month_of_year = factor(month_of_year, levels = 1:12, labels = month_names))

#Call View() to check that the columns were successfully created
View(all_data_2)

#Start cleaning the data to get rid of null values, duplicates and cases where the ride length is less then zero
all_data_2 <- all_data_2 %>% drop_na()
all_data_2 <- all_data_2 %>% distinct()
all_data_2 <- all_data_2 %>% filter(ride_length > 0)

#This command will convert the values from scientific notation to whole numbers for our graphs
options(scipen = 999)

#Analysis

#Mean, median, max and min of all rides
mean(all_data_2$ride_length)
median(all_data_2$ride_length)
max(all_data_2$ride_length)
min(all_data_2$ride_length)

#Number of trips by everyone, members and casual riders
nrow(all_data_2)
nrow(all_data_2 %>% filter(member_casual == "member"))
nrow(all_data_2 %>% filter(member_casual == "casual"))

#Graph of Number of Trips by Type of Member
all_data_2 %>% 
  group_by(member_casual) %>%
  dplyr::summarize(count_trips = n()) %>%
  ggplot(aes(x=member_casual, y=count_trips, fill = member_casual)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Trips by Type of Member", x = "Type of Member", y = "Number of Trips")

#Number of trips by type of bicycle
nrow(all_data_2 %>% filter(rideable_type == "docked_bike"))
nrow(all_data_2 %>% filter(rideable_type == "classic_bike"))
nrow(all_data_2 %>% filter(rideable_type == "electric_bike"))

#Graph of Number of Trips by Type of Bicycle
all_data_2 %>%
  group_by(rideable_type) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x=rideable_type, y=count_trips, fill = rideable_type)) +
  geom_bar(stat='identity') +
  labs(title="Number of Trips by Type of Bicycle", x="Type of Bicycle", y="Number of Trips")

#Number of trips by type of bicycle by member or casual
nrow(all_data_2 %>% filter(rideable_type == "docked_bike",member_casual == "member"))
nrow(all_data_2 %>% filter(rideable_type == "classic_bike",member_casual == "member"))
nrow(all_data_2 %>% filter(rideable_type == "electric_bike",member_casual == "member"))
nrow(all_data_2 %>% filter(rideable_type == "docked_bike",member_casual == "casual"))
nrow(all_data_2 %>% filter(rideable_type == "classic_bike",member_casual == "casual"))
nrow(all_data_2 %>% filter(rideable_type == "electric_bike",member_casual == "casual"))


#Graph of Number of Trips by Type of Bicycle by Type of Member
all_data_2 %>%
  group_by(rideable_type, member_casual) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x=rideable_type, y=count_trips, fill=member_casual)) +
  geom_bar(stat='identity', position = "dodge") +
  labs(title="Number of Trips by Type of Bicycle by Type of Member", x="Type of Bicycle", y="Number of Trips")

#Number of Trips by Day of 
print(all_data_2 %>% group_by(day_of_week) %>% summarize(count_trips = n()))
#Number of Trips by Day of week by the type of member
print(all_data_2 %>% group_by(day_of_week, member_casual) %>% summarize(count_trips = n()))

#Graph of Number of Trips by Day of Week by Type of Member
all_data_2 %>%
  group_by(day_of_week, member_casual) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x=day_of_week, y=count_trips, fill=member_casual)) +
  geom_bar(stat='identity', position = "dodge") +
  labs(title="Number of Trips by Day of Week by Type of Member", x="Day of the Week", y="Number of Trips")

#Number of Trips by Month
print(all_data_2 %>% group_by(month_of_year) %>% summarize(count_trips = n()))
#Number of Trips by Month by the type of member
data <- all_data_2 %>% group_by(month_of_year, member_casual) %>% summarize(count_trips = n())
print(data$month_of_year)
print(data$member_casual) 
print(data$count_trips)

#Graph of Number of Trips by Month of Year by Type of Member
all_data_2 %>%
  group_by(month_of_year, member_casual) %>%
  dplyr::summarize(count_trips = n()) %>%  
  ggplot(aes(x=month_of_year, y=count_trips, fill=member_casual)) +
  geom_bar(stat='identity', position = "dodge") +
  labs(title="Number of Trips by Month of Year by Type of Member", x="Month of the Year", y="Number of Trips")
