library(tidyverse)
library(lubridate)
library(dplyr)
library(readr)

dailyActivity <- read_csv("D:/R projects/dailyActivity_merged.csv")
sleepDay <- read_csv("D:/R projects/sleepDay_merged.csv")
weightInfo <- read_csv("D:/R projects/weightLogInfo_merged.csv")

# check unique IDs
n_distinct(dailyActivity$Id)
n_distinct(sleepDay$Id)
n_distinct(dailyActivity$Id)

# check for na values and remove duplicates
sum(is.na(dailyActivity))
sum(duplicated(dailyActivity))
dailyActivity <- dailyActivity[!duplicated(dailyActivity),]
sum(is.na(sleepDay))
sum(duplicated(sleepDay))
sleepDay <- sleepDay[!duplicated(sleepDay),]

#summary of active minutes activity
dailyActivity %>% 
  select(TotalSteps, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes, Calories) %>%
  summary()

#summary of sleep data set
sleepDay %>% 
  select(TotalMinutesAsleep, TotalTimeInBed) %>% 
  summary()
  
#converted dates into weekdays
dailyActivity <- dailyActivity %>% 
  mutate(Weekday = weekdays(as.Date(ActivityDate, "%m/%d/%Y")))

#added column for sum of activity minutes
activeMinutes<-rowSums(dailyActivity[,c('VeryActiveMinutes','FairlyActiveMinutes','LightlyActiveMinutes')])

#added ratio of ActiveMin/SedentaryMin rounded to 3 points
dailyActivity <- dailyActivity %>% 
  mutate(ratio = activeMinutes/SedentaryMinutes)
dailyActivity$ratio_rounded <- round(dailyActivity$ratio, digit = 3)

#created new column for merging (merged_data)
dailyActivity$date <- as.Date(dailyActivity$ActivityDate, format='%m/%d/%Y')
sleepDay$date <- as.Date(sleepDay$SleepDay, format="%m/%d/%Y")

#created merged data for activity and sleep
merged_data <- merge(sleepDay, dailyActivity, by=c('Id', 'date'))

#ran into the issues of inf
summary(dailyActivity$ratio_rounded)

#changed inf to na values
dailyActivity <- do.call(data.frame,
                         lapply(dailyActivity,
                                function(x) replace(x, is.infinite(x), NA)))
#dropped one value of NA
dailyActivity <- na.omit(dailyActivity)


#histogram for ratio_rounded
hist(dailyActivity$ratio_rounded, 
     xlab = 'Ratio of Active Minutes to Sedentary Minutes',
     breaks = c(0.0, seq(0.1, 2.0, 0.1)),
     main = 'Ratio_Rounded to Frequency')

#barchart for days of activity
ggplot(dailyActivity)+
geom_bar(mapping=aes(x=Weekday), fill = 'blue') +
  labs(title = 'Activity Frequency by Weekday')

#barchart for comparing Steps for each day
ggplot(merged_data)+
  geom_col(mapping=aes(x=Weekday,y = TotalSteps), fill = 'red') +
  labs(title = 'Total Steps per Weekday')

#scatterplot for minutes asleep vs sedentary minutes
ggplot(data = merged_data, aes(x=TotalMinutesAsleep, y=SedentaryMinutes))+
  geom_point() + geom_smooth() + labs(title = 'Total Minutes Asleep vs. Sedentary Minutes')

cor(merged_data$SedentaryMinutes,merged_data$TotalMinutesAsleep)
