#Install the packages and load the library required for analysis
install.packages("ggplot2")
install.packages("dplyr")
library(ggplot2)
library(dplyr)
# ----------------------------------------------------------------------------------
#Sourcing the data using read.csv
uber_data <- read.csv('Uber Request Data.csv', stringsAsFactors = FALSE)

# ----------------------------------------------------------------------------------
# Data Clean Up
#Request.timestamp has 2 different date format in the data set
#Create 2 new columns to handle the 2 different date formats
uber_data$Request.timestamp1 <- as.POSIXct(uber_data$Request.timestamp, format = "%d/%m/%Y %H:%M")
uber_data$Request.timestamp2 <- as.POSIXct(uber_data$Request.timestamp, format = "%d-%m-%Y %H:%M:%S")
#Merge the dates in to first column
uber_data$Request.timestamp1[is.na(uber_data$Request.timestamp1)] <- uber_data$Request.timestamp2[!is.na(uber_data$Request.timestamp2)]
#copy the dates into actual column of Request.timestamp
uber_data$Request.timestamp <- uber_data$Request.timestamp1
#Delete the extra columns created
uber_data <- uber_data[,-(7:8)]


#Drop.timestamp has 2 different date format in the data set
#Create 2 new columns to handle the 2 different date formats
uber_data$Drop.timestamp1 <- as.POSIXct(uber_data$Drop.timestamp, format = "%d/%m/%Y %H:%M")
uber_data$Drop.timestamp2 <- as.POSIXct(uber_data$Drop.timestamp, format = "%d-%m-%Y %H:%M:%S")
#Merge the dates in to first column for records with Status as Trip Completed
uber_data$Drop.timestamp1[is.na(uber_data$Drop.timestamp1[uber_data$Status == "Trip Completed"])] <- uber_data$Drop.timestamp2[!is.na(uber_data$Drop.timestamp2[uber_data$Status == "Trip Completed"])] 
#copy the dates into actual column of Drop.timestamp
uber_data$Drop.timestamp <- uber_data$Drop.timestamp1
#Delete the extra columns created
uber_data <- uber_data[,-(7:8)]

# ----------------------------------------------------------------------------------
# Create new columns- Derived metrics required for analysis

#Add new column for hour during which the request was made
uber_data$request_hour <- as.numeric(format(uber_data$Request.timestamp, "%H"))
uber_data$weekday <- weekdays(as.POSIXct(uber_data$Request.timestamp), abbreviate = F)

#bivariate Analysis on the Weekdays
#Plot 1 a: weekday wise requests plot
ggplot(uber_data,aes(x=factor(weekday),fill=factor(Pickup.point)))+
  geom_bar(stat='count',position = "dodge") +ggtitle("Weekday wise Demand for Uber Cabs")+labs(x="Weekday", y="Number Requests")+
  labs(fill="Pickup Point")

#The overall trend of requests does not vary much based on the day of the week. We can assume that the data given does not vary based on which day of the week

#bivariate Analysis on the request hour
#Plot 1 b: Hour wise requests plot
ggplot(uber_data,aes(x=factor(request_hour),fill=factor(Pickup.point)))+
  geom_bar(stat='count',position = "dodge") +ggtitle("Hour wise Demand for Uber Cabs") +labs(x="Request Hour", y="Number Requests")+
  labs(fill="Pickup Point")

#Based on the analysis on request hour, we can see mainly 5 variations in the no of requests
#we can classify hours in to 5 different time slots

uber_data$Timings[uber_data$request_hour<= 4]<- "Early_Morning"
uber_data$Timings[uber_data$request_hour > 4  & uber_data$request_hour <=10 ] <- "Morning_Peak"
uber_data$Timings[uber_data$request_hour >10 & uber_data$request_hour <=17]<- "Day_time"
uber_data$Timings[uber_data$request_hour >17 & uber_data$request_hour <22]<- "Evening_Peak"
uber_data$Timings[uber_data$request_hour>= 22]<- "Late_night"

# ----------------------------------------------------------------------------------
#Creating plots in R
#Plot 2: plot to check the requests across different time slots along with trip status
ggplot(uber_data,aes(x=factor(Timings),fill=factor(Status)))+geom_bar(stat="count")+ggtitle("Time Slot wise Demand for Uber Cabs")+labs(x="Timings", y="Number of Requests")+  labs(fill="Status")

# Based on plot 1 and plot 2 we can see that there is high no of cancellations in morning peak hours and shortage of cabs during evening peak hours:
#create subsets for moring and evening peak hours for further analysis:
morning_data <- subset(uber_data,uber_data$Timings=="Morning_Peak")
evening_data <- subset(uber_data,uber_data$Timings=="Evening_Peak")

#Plot 3:Morning Peak hours Cab Status
  ggplot(morning_data,aes(x=factor(Status),fill=factor(Pickup.point)))+geom_bar(stat="count",position = "stack") +ggtitle("Morning Peak hours Demand for Uber Cabs") + labs(x="Status",y="No. of Requests")+labs(fill="Pickup Point")

#Plot 4 :Evening Peak hours Cab Status
  ggplot(evening_data,aes(x=factor(Status),fill=factor(Pickup.point)))+geom_bar(stat="count",position = "stack")+ggtitle("Evening Peak hours Demand for Uber Cabs") +
    labs(x="Status",y="No. of Requests")+
    labs(fill="Pickup Point")
  
  
# ---------------------------------------------------------------------------------- 
# Export Dataset to csv which can be used in Tableau for additional plots required for presentation
write.csv(uber_data, "uber_data.csv")  
  
#Plots created in R are for basic understanding and analysis of the data. Plots used in presentation are created in Tableau
  
# ---------------------------------------------------------------------------------- 
  
