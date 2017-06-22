#Load the uber request data file into R. 
#Don't convert the strings into factors
uber_masterdata <- read.csv("Uber request data.csv",stringsAsFactors = FALSE,na.strings = TRUE)
#Extract the first two characters of Request time column 
#Save it as a seperate column of the dataframe
uber_masterdata$request_hour <- sub(":.*","",uber_masterdata$Request.time)
#convert the separated charcters to data type numeric
uber_masterdata$request_hour <- as.numeric(uber_masterdata$request_hour)
#load the required packages 
require(dplyr)
require(ggplot2)
require(scales)
# Plot the number of cabs requested in a particular hour for all 05 days
# Pickup points will be displayed in two colors
hourwise_request_count <- ggplot(uber_masterdata,aes(x=factor(request_hour),fill=factor(Pickup.point)))
#Add title and lables to the plot and save it as a object
plot1 <- hourwise_request_count+geom_bar(stat='count',position = "dodge")+
              ggtitle("Hourly Demand for Uber Cabs")+
                labs(x="Time in Hours", y="Number of Cabs Requested")+
                  labs(fill="Pickup Point")
                    
#view the plot
plot1
# Generate a sequence of numbers from 0 to 23 
# save it as a vector
request_hour <- c(0:23)
# create a vector of names of time slots
Time_Slot1 <- c("Pre_Morning","Morning_Rush","Day_Time","Evening_Rush","Late_Night")
# create a vector which represents the number of times time slots are to be repeated
times <- c(4,6,7,5,2)
# Repeat the time slots number of times required and save it as a vector
# The number of elements in this vector should correspond to 24
Time_Slot <- rep(Time_Slot1,times)
# create a new dataframe with sequence of number generated and time slots
new_frame <- data.frame(Time_Slot,request_hour)
#Merge the main uber request dataframe with the new dataframe created
uber_masterdata <- merge(uber_masterdata,new_frame,by="request_hour",all.x=TRUE)
#Change the sequence of columns of dataframe
uber_masterdata <- uber_masterdata[,c(2,3,4,5,6,7,8,1,9)]
#Subset the master dataframe.
#Subsetted dataframe should only consist of Trips completed
trips_completed <- subset(uber_masterdata,uber_masterdata$Status=="Trip Completed")
#Plot a bar chart with time-slots on x axis and trips completed on Y-axis
Timeslot_bar <- ggplot(trips_completed,aes(x=Time_Slot))
plot2 <- Timeslot_bar+geom_bar(stat="count",col="black",fill="green")+
          ggtitle("Trips completed during different Time Slots")+
            labs(x="Time Slots",y="Trips Completed")+
              geom_text(stat='count',aes(label=..count..),vjust=-1)+
                guides(fill=FALSE)+
                  scale_x_discrete(limits=c("Morning_Rush","Evening_Rush","Day_Time",
                                            "Late_Night","Pre_Morning"))
      
                    
#view the plot
plot2

#plot a bar chart with time slots on x-axis and request frequency on y-axis
# show the status of requests in different colors, add title, axis labels 
# save the plot as a object
timeslot_request_count <- ggplot(uber_masterdata,aes(x=factor(Time_Slot),fill=factor(Status)))
plot3 <- timeslot_request_count+geom_bar(stat="count",position = "stack",col="black")+
          ggtitle("Trips during Different Time Slots")+
            scale_x_discrete(limits=c("Evening_Rush","Morning_Rush","Day_Time",
               "Late_Night","Pre_Morning"))+
              labs(x="Time Slots",y="Number of Requests")+labs(fill="Trip Status")+
                scale_fill_discrete(limits=c("Trip Completed","No Cars Available","Cancelled"))
              
#view the plot
plot3

#problem 1. Large number of service requests got cancelled during the Morning_Rush Time slot
#Subset the Morning Rush time slot data for analysis
Problem_df <- subset(uber_masterdata,uber_masterdata$Time_Slot=="Morning_Rush")
#Plot the bargraph with status of request in x-axis and count in y-axis for Morning rush time slot
#Show the request from different pickup points in different colors
Problem1_count <- ggplot(Problem_df,aes(x=factor(Status),fill=factor(Pickup.point)))
plot4 <- Problem1_count+geom_bar(stat="count",position = "stack")+
  ggtitle("Morning Rush Cab Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("Trip Completed","Cancelled","No Cars Available"))+
  annotate("text", x=-Inf,y=Inf,label="Airport - 2.88% & City = 97.11%", hjust=-.1,vjust=1)
#view the plot
plot4

#Number of trips cancelled for the Morning rush time slot
total_trip_cancel <- length(which(Problem_df$Status=="Cancelled"))
#Number of trips cancelled from airport for Morning rush
airport_trip_cancel <- length(which((Problem_df$Pickup.point=="Airport") & (Problem_df$Status == "Cancelled")))
# Number of trips cancelled from city for Morning rush
city_trip_cancel <- length(which((Problem_df$Pickup.point=="City") & (Problem_df$Status == "Cancelled")))
# Percentage of trips cancelled from city out of total trips cancelled during morning rush
percent_trip_cancel_city <- (city_trip_cancel/total_trip_cancel*100)
# Percentage of trips cancelled from airport out of total trips cancelled during Morning rush
percent_trip_cancel_airport <- (airport_trip_cancel/total_trip_cancel*100)
# Number of trips requested from city to airport during morning rush
demand_trip_request_city <- length(which(Problem_df$Pickup.point=="City"))
#Number of trips completed from city to airport during morning rush
demand_trip_city_completed <- length(which((Problem_df$Pickup.point=="City")& (Problem_df$Status=="Trip Completed")))





#problem2
#subset the data for Evening rush from dataframe for analysis
Problem2_df <- subset(subset(uber_masterdata,uber_masterdata$Time_Slot=="Evening_Rush"))
#plot the bar graph with status of requests on x-axis and count in y-axis for evening rush time slot
# Show the request from different pickup points in different colors
Problem2_count <- ggplot(Problem2_df,aes(x=factor(Status),fill=factor(Pickup.point)))
plot5 <- Problem2_count+geom_bar(stat="count",position = "stack")+
  ggtitle("Evening Rush Cabs Status")+
  labs(x="Status",y="Total count")+
  labs(fill="Pickup Point")+scale_x_discrete(limits=c("No Cars Available","Trip Completed","Cancelled"))+
  annotate("text", x=-Inf,y=Inf,label="Airport - 95.41% & City = 4.59%", hjust=-.1,vjust=1)  
#view the plot
plot5



# No of service requests with no cars available for evening rush time slot
total_nocar_available <- length(which(Problem2_df$Status=="No Cars Available"))
# No of  service requests with no cars available from airport during evening rush
airport_nocar_available <- length(which((Problem2_df$Pickup.point=="Airport") & (Problem2_df$Status == "No Cars Available")))
# No of service requests with no cars availablefrom city during evening rush
city_nocar_available <- length(which((Problem2_df$Pickup.point=="City") & (Problem2_df$Status == "No Cars Available")))
# Percentage of no cars available status from city out of total no cars available during evening rush
percent_city_nocar <- (city_nocar_available/total_nocar_available*100)
# Percentage of no cars available status from airport out of total no cars available during evening rush
percent_airport_nocar <- (airport_nocar_available/total_nocar_available*100)
#No of service requests from airport to city during evening rush
demand_nocar_request_airport <- length(which(Problem2_df$Pickup.point=="Airport"))
#No of trips completed from airport to city during evening rush
demand_nocar_request_airport_completed <- length(which((Problem2_df$Pickup.point=="Airport") & (Problem2_df$Status=="Trip Completed")))






#_____________________________________________________________________________________
# Practice code
#?lapply
# as.character(uber_masterdata$request_hour)
# typeof(uber_masterdata$request_hour)
# class(uber_masterdata$request_hour)
# 
# 
# slot_replace <- function( )(
#   if(x >= 0 & x <= 3) 
#     {
#     print( "Pre_Morning" )
#     } else if (x >= 04 & x <= 09) 
#     {
#     print( "Morning_Rush" )
#     } else if (x >= 10 & x <= 16) 
#     {
#     print( "Day_Time" )
#     } else if (x >= 17 & x <= 21) 
#     {
#     print( "Evening_Rush" )
#     } else
#     print( "Late_Night" )
# )  
# 
# 
# Time_Slot = NULL
#   
# for (i in uber_masterdata$request_hour) {
#      if(uber_masterdata$request_hour == "00" | uber_masterdata$request_hour == "1" | uber_masterdata$request_hour == "2" | uber_masterdata$request_hour == "3") 
#   {
#     Time_Slot[i]== c( "Pre_Morning" )
#   } else if (uber_masterdata$request_hour == "04" | uber_masterdata$request_hour == "05" | uber_masterdata$request_hour == "06" | uber_masterdata$request_hour == "07"|uber_masterdata$request_hour == "08" | uber_masterdata$request_hour == "09") 
#   {
#     Time_Slot[i]== c( "Morning_Rush" )
#   } else if (uber_masterdata$request_hour == "10" | uber_masterdata$request_hour == "11" | ) 
#   {
#     Time_Slot[i]== c( "Day_Time" )
#   } else if (uber_masterdata$request_hour == '17' | uber_masterdata$request_hour == '21') 
#   {
#     Time_Slot[i]== c( "Evening_Rush" )
#   } else
#     Time_Slot[i]== c( "Late_Night" )
# }
# 
# Time_Slot <- apply(uber_masterdata,2,slot_replace(request_hour))



# uber_masterdata <- read.csv("Uber request data.csv",stringsAsFactors = FALSE,na.strings = TRUE)
# summary(uber_masterdata)
# class(uber_masterdata$Request.time)
# request <-as.data.frame(sub(":.*","",uber_masterdata$Request.time),stringsAsFactors = FALSE)
# names(request)[1] <- "request_hour"
# class(request_hour)
# typeof(request_hour)
# uber_masterdata <- cbind.data.frame(uber_masterdata,request)
# str(uber_masterdata)
# uber_masterdata$request_hour <- as.numeric(uber_masterdata$request_hour)
# str(uber_masterdata)
# require(dplyr)
# require(ggplot2)
# # unique(uber_masterdata$Pickup.point)
# # unique(is.na(uber_masterdata$Pickup.point))
# hourwise_request_count <- ggplot(uber_masterdata,aes(x=factor(request_hour),fill=factor(Pickup.point)))
# plot1 <- hourwise_request_count+geom_bar(stat="count",position = "dodge")+ggtitle("Hourly Demand for Uber Cabs")+labs(x="Time in Hours", y="Number of Cabs Requested")
# plot1
# # class(uber_masterdata$request_hour)
# # as.numeric(uber_masterdata$request_hour)
# # class(uber_masterdata$request_hour)
# 
# 
# 
# uber_masterdata$Time_Slot[uber_masterdata$request_hour <= 3] <- c("Pre_Morning")
# 
# uber_masterdata$Time_Slot[((uber_masterdata$request_hour >= 4) & 
#                              (uber_masterdata$request_hour <= 9))] <- c("Morning_Rush")
# 
# uber_masterdata$Time_Slot[((uber_masterdata$request_hour >= 10) & 
#                              (uber_masterdata$request_hour <= 16))] <- c("Day_Time")
# 
# uber_masterdata$Time_Slot[((uber_masterdata$request_hour >= 17) & 
#                              (uber_masterdata$request_hour <= 21))] <- c("Evening_Rush")
# 
# uber_masterdata$Time_Slot[which(is.na(uber_masterdata$Time_Slot==T))] <- c("Late_Night")
# 
# #     
# # 
# Day_Time_count <- length(which((uber_masterdata$Time_Slot=="Day_Time") & (uber_masterdata$Status == "Trip Completed")))
# Pre_Morning_count <- length(which((uber_masterdata$Time_Slot=="Pre_Morning") & (uber_masterdata$Status == "Trip Completed")))
# Morning_Rush_count <- length(which((uber_masterdata$Time_Slot=="Morning_Rush") & (uber_masterdata$Status == "Trip Completed")))
# Evening_Rush_count <- length(which((uber_masterdata$Time_Slot=="Evening_Rush") & (uber_masterdata$Status == "Trip Completed")))
# Late_Night_count <- length(which((uber_masterdata$Time_Slot=="Late_Night") & (uber_masterdata$Status == "Trip Completed")))
#__________________________________________________________________________________________________________________________________________
