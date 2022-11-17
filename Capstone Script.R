#loaded packages that I planned to use 
library(tidyverse) 
library(lubridate)  
library(ggplot2) 
library(dplyr)

#set working directory to correct fiel location
setwd("C:/Users/ekabu/OneDrive/Documents/Data") 

#importing data to r
q1_2019 <- read_csv("Divvy_Trips_2019_Q1.csv") 
q2_2019 <- read_csv("Divvy_Trips_2019_Q2.csv")
q3_2019 <- read_csv("Divvy_Trips_2019_Q3.csv")
q4_2019 <- read_csv("Divvy_Trips_2019_Q4.csv")

#The data was separated by fiscal quarters, so first merging them all into one 

#checking column names
colnames(q1_2019) 
colnames(q2_2019)
colnames(q3_2019)
colnames(q4_2019)

#column names for q2 are different, so renaming them to match all other column names
q2_2019 <- rename(q2_2019,
       trip_id = "01 - Rental Details Rental ID",
       start_time = "01 - Rental Details Local Start Time",
       end_time =  "01 - Rental Details Local End Time",
       bikeid = "01 - Rental Details Bike ID",
       tripduration = "01 - Rental Details Duration In Seconds Uncapped",
       from_station_id = "03 - Rental Start Station ID",
       from_station_name = "03 - Rental Start Station Name",
       to_station_id = "02 - Rental End Station ID",
       to_station_name = "02 - Rental End Station Name",
       usertype = "User Type",
       gender = "Member Gender",
       birthyear = "05 - Member Details Member Birthday Year")

#making sure data types are consistent
str(q1_2019)   
str(q2_2019)
str(q3_2019)
str(q4_2019)

#converting trip_id and bikeid to characters so they can stack correctly
q1_2019 <- mutate(q1_2019, trip_id = as.character(trip_id),
                  bikeid = as.character(bikeid))
q2_2019 <- mutate(q2_2019, trip_id = as.character(trip_id),
                  bikeid = as.character(bikeid))
q3_2019 <- mutate(q3_2019, trip_id = as.character(trip_id),
                                    bikeid = as.character(bikeid))
q4_2019 <- mutate(q4_2019, trip_id = as.character(trip_id),
                                    bikeid = as.character(bikeid))

#merging into one data frame
all_trips_2019 <- bind_rows(q1_2019, q2_2019, q3_2019, q4_2019) 


dim(all_trips_2019) #inspecting the data
summary(all_trips_2019)

#adding date, day, month, and year columns to provide additional opportunities
#to aggregate the data
all_trips_2019$date <- as.Date(all_trips_2019$start_time)
all_trips_2019$month <- format(as.Date(all_trips_2019$date), "%m")
all_trips_2019$day <- format(as.Date(all_trips_2019$date), "%d")
all_trips_2019$year <- format(as.Date(all_trips_2019$date), "%Y")
all_trips_2019$day_of_week <- format(as.Date(all_trips_2019$date), "%A")

#checking the summary statistics for the entire data set
summary(all_trips_2019$tripduration)

#comparing summary statistics between customers and subscribers
aggregate(all_trips_2019$tripduration, by=list(all_trips_2019$usertype), FUN=mean)
aggregate(all_trips_2019$tripduration ~ all_trips_2019$usertype, FUN = median)
aggregate(all_trips_2019$tripduration ~ all_trips_2019$usertype, FUN = max)
aggregate(all_trips_2019$tripduration ~ all_trips_2019$usertype, FUN = min)

#sorting by day of the week
aggregate(all_trips_2019$tripduration ~ all_trips_2019$usertype +
                all_trips_2019$day_of_week, FUN = mean)

#days of the week were out of order so I put them in the correct order
all_trips_2019$day_of_week <- 
        ordered(all_trips_2019$day_of_week, 
        levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
        aggregate(all_trips_2019$tripduration ~ all_trips_2019$usertype + 
        all_trips_2019$day_of_week, FUN = mean)

all_trips_2019 %>% mutate(weekday = wday(start_time, label = TRUE)) %>% # creates weekday field 
group_by(usertype, weekday) %>% #groups by usertype and weekday
summarise(number_of_rides = n(), average_duration = mean(tripduration)) %>% # calculates number of rides and average duration
arrange(usertype, weekday) #sorts by user type and weekday

#visualize number of rides each day by rider type
all_trips_2019 %>% mutate(weekday = wday(start_time, label = TRUE)) %>%  
        group_by(usertype, weekday) %>% 
        summarise(number_of_rides = n(), average_duration = mean(tripduration)) %>% 
        arrange(usertype, weekday) %>% 
        ggplot(aes(x = weekday, y = number_of_rides, fill = usertype)) + 
        geom_col(position = "dodge")


#visualize average duration by rider type
all_trips_2019 %>% mutate(weekday = wday(start_time, label = TRUE)) %>%  
        group_by(usertype, weekday) %>% 
        summarise(number_of_rides = n(), average_duration = mean(tripduration)) %>% 
        arrange(usertype, weekday) %>% 
        ggplot(aes(x = weekday, y = average_duration, fill = usertype)) + 
        geom_col(position = "dodge")

#visualize number of rides by gender
all_trips_2019 %>%  
        group_by(usertype, gender) %>% 
        summarise(number_of_rides = n()) %>% 
        arrange(usertype, gender) %>% 
        ggplot(aes(x = usertype, y = number_of_rides, fill = gender)) + 
        geom_col(position = "dodge")

        

