library(ggplot2)
library(zoo)
library(lubridate)
library(stringr)
library(chron)
######################################################################
# HW3.21
######################################################################

PW = read.csv("/Users/appobs/Desktop/hw/465/week7/hw3/datasets/PortlandWaterLevel2003.csv")
head(PW)
# mydate = as.Date(PW$Date, format = "%m/%d/%Y")
# mytime = as.Date(PW$Time, format = "%H:%M:%S")
mytime = strptime(PW$Time, format="%H:%M:%S")
mytime = as.POSIXct(PW$Time)
# First, we convert it to a time series object.  There are several in R, but one 
# that is most fully featured is the "zoo" object.  
myWL = PW$WL
toWldata = zoo(myWL, mydate)
toWltime = zoo(myWL, mytime)
# Note that toWl does not have a "Date" column, the dates are the labels on the
# entries
head(toWldata)
head(toWltime)
head(time(toWldata))
head(time(toWltime))
# Now we turn it into a time series object.  The base R functionality knows how
# to deal with a time series ... it plots it by default with lines!
plot(toWl)
plot(mydate, toWl, type = "l")
# The "time(toWl)" recovers a toWl time column, so if we want to plot it
# with ggplot, we need to use "time(toWl)" for the x-values
ggplot(toWl, aes(x=time(toWl), y=myWL)) + geom_line()

######################################################################
# HW3.32 RosePlot
######################################################################
crimeData = read.csv("/Users/appobs/Desktop/hw/465/week7/hw3/datasets/ChicagoCrime2018.csv")
head(crimeData)

# Convert the date to a POSIX datetime field.  Note, however that there are 
# two of these (POSIXlt and POSIXct) and ggplot only likes
# POSIXct or "calendar time" rather than "local time".  Unfortunately the 
# best way to convert, goes straight to POSIXlt ... so we need to convert 
# afterwards

crimeData$to24hours = strptime(crimeData$Date, format="%m/%d/%Y %I:%M:%S %p")
crimeData$to24hours = as.POSIXct(crimeData$to24hours)
crimeData$Date = strptime(crimeData$Date, format="%m/%d/%Y %H:%M:%S")
crimeData$Date = as.POSIXct(crimeData$Date)

# Get a summary.  We've got a bunch of time information
summary(crimeData)
head(crimeData)

# Plot the crime 
plot(crimeData$Date, crimeData$Primary.Type)
# Get parts of the date-time
hour = hour(crimeData$Date)
day = day(crimeData$Date)
month = month(crimeData$Date)
# split the data as month and hour
head(crimeData$Date)

#MonthAndHours = str_split_fixed(as.character(crimeData$Date), " ", 2)
#head(MonthAndHours)
#str(MonthAndHours)
MonthAnd24Hours = str_split_fixed(as.character(crimeData$to24hours), " ", 2)
head(MonthAnd24Hours)
# combine this to the original dataset,
crimeData = cbind(crimeData, MonthAndHours, MonthAnd24Hours)
names(crimeData)[34] = "SplitMs"
names(crimeData)[35] = "SplitHrs"
head(crimeData)
str(crimeData)
# extract the hour and month and rename the column
Month = str_split_fixed(as.character(crimeData$SplitMs), "-", 3)
head(Month)
hours = str_split_fixed(as.character(crimeData$SplitHrs), ":", 3)
head(hours)
crimeData = cbind(crimeData, Month, hours)
str(crimeData)
names(crimeData)[36] = "year"
names(crimeData)[37] = "month"
names(crimeData)[38] = "days"
names(crimeData)[39] = "hours"
names(crimeData)[40] = "mins"
names(crimeData)[41] = "sec"
head(crimeData)


# Now, let's make a rose-plot, first subset the types
assaults = subset(crimeData, Primary.Type == "ASSAULT")
KIDNAPPING = subset(crimeData, Primary.Type == "KIDNAPPING")
NARCOTICS = subset(crimeData, Primary.Type == "NARCOTICS")
THEFT = subset(crimeData, Primary.Type == "THEFT")

head(assaults)
head(KIDNAPPING)
head(NARCOTICS)
head(THEFT)
# Don't do the subsetting inside the ggplot command (fill = ...).  
# That fill command is making it try to stack the bars and is 
# causing you to process much more data than you need

# For the month pattern
ggplot(assaults, aes(x=month, fill=Primary.Type)) +
  geom_bar(breaks = seq(0, 12), width = 1, color = "grey") +
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("count") + ggtitle("assaults by time of month") +
  scale_x_yearmon("", limits = c(0, 12), breaks = seq(0, 12), labels = seq(0,12))

ggplot(KIDNAPPING, aes(x=month, fill=Primary.Type, color = "red")) +
  geom_bar(breaks = seq(0, 12), width = 1, color = "grey") +
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("count") + ggtitle("KIDNAPPING by time of month") +
  scale_x_yearmon("", limits = c(0, 12), breaks = seq(0, 12), labels = seq(0,12))

ggplot(NARCOTICS, aes(x=month, fill=Primary.Type)) +
  geom_bar(breaks = seq(0, 12), width = 1, color = "grey") +
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("count") + ggtitle("NARCOTICS by time of month") +
  scale_x_yearmon("", limits = c(0, 12), breaks = seq(0, 12), labels = seq(0,12))

ggplot(THEFT, aes(x=month, fill=Primary.Type)) +
  geom_bar(breaks = seq(0, 12), width = 1, color = "grey") +
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("count") + ggtitle("assaults by time of month") +
  scale_x_yearmon("", limits = c(0, 12), breaks = seq(0, 12), labels = seq(0,12))

###########################################
# For the 24hours pattern
###########################################

ggplot(assaults, aes(x=hours, fill=Primary.Type)) +
  geom_bar(breaks = seq(0, 24), width = 1, color = "grey") +
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("count") + ggtitle("assaults by time of hours") +
  scale_x_yearmon("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))

ggplot(KIDNAPPING, aes(x=hours, fill=Primary.Type, color = "red")) +
  geom_bar(breaks = seq(0, 24), width = 1, color = "grey") +
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("count") + ggtitle("KIDNAPPING by time of hours") +
  scale_x_yearmon("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))

ggplot(NARCOTICS, aes(x=hours, fill=Primary.Type)) +
  geom_bar(breaks = seq(0, 24), width = 1, color = "grey") +
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("count") + ggtitle("NARCOTICS by time of hours") +
  scale_x_yearmon("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))

ggplot(THEFT, aes(x=hours, fill=Primary.Type)) +
  geom_bar(breaks = seq(0, 24), width = 1, color = "grey") +
  coord_polar(start = 0) + theme_minimal() +
  scale_fill_brewer() + ylab("count") + ggtitle("assaults by time of hours") +
  scale_x_yearmon("", limits = c(0, 24), breaks = seq(0, 24), labels = seq(0,24))

write.csv(crimeData, "crimeData.csv", row.names=F)