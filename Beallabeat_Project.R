## Installing Tidyverse package.

install.packages("tidyverse")
install.packages("here")
install.packages("skimr")
install.packages("janitor")

#Loading the relevant Packages.

library(tidyverse)
library(lubridate)
library(here)
library(skimr)
library(janitor)

## Importing the data sets.

Daily_Activity <- read.csv(
  "Bellabeat_project/dailyActivity_merged.csv")
Hourly_Calories <- read.csv(
  "Bellabeat_project/hourlyCalories_merged.csv")
Hourly_Intensities <- read.csv(
  "Bellabeat_project/hourlyintensities_merged.csv")
Sleep_Day <- read.csv(
  "Bellabeat_project/sleepDay_merged.csv")
Weight_Log_Info <- read.csv(
  "Bellabeat_project/weightLogInfo_merged.csv")

## Taking a view of the data sets to ensure everything was imported properly with the head(), str() and glimpse() functions.

head(Daily_Activity)
head(Hourly_Calories)
head(Hourly_Intensities)
head(Sleep_Day)
head(Weight_Log_Info)

str(Daily_Activity)
str(Hourly_Calories)
str(Hourly_Intensities)
str(Sleep_Day)
str(Weight_Log_Info)

glimpse(Daily_Activity)
glimpse(Hourly_Calories)
glimpse(Hourly_Intensities)
glimpse(Sleep_Day)
glimpse(Weight_Log_Info)

## Data cleaning: splitting date and time from ActivityHour column in Hourly_Caloroies and Hourly_Intensities.

Hourly_Calories$ActivityHour=as.POSIXct(
  Hourly_Calories$ActivityHour,
  format="%m/%d/%Y %I:%M:%S %p", 
  tz=Sys.timezone())
Hourly_Calories$time <- format(
  Hourly_Calories$ActivityHour, 
  format = "%H:%M:%S")
Hourly_Calories$date <- format(
  Hourly_Calories$ActivityHour, 
  format = "%m/%d/%y")

Hourly_Intensities$ActivityHour=as.POSIXct(
  Hourly_Intensities$ActivityHour, 
  format="%m/%d/%Y %I:%M:%S %p", 
  tz=Sys.timezone())
Hourly_Intensities$time <- format(
  Hourly_Intensities$ActivityHour, 
  format = "%H:%M:%S")
Hourly_Intensities$date <- format(
  Hourly_Intensities$ActivityHour, 
  format = "%m/%d/%y")

#Using the skim_without_charts() to  view a summary of the data and the column types, this is to verify the changes above.

skim_without_charts(Daily_Activity)
skim_without_charts(Hourly_Calories)
skim_without_charts(Hourly_Intensities)
skim_without_charts(Sleep_Day)
skim_without_charts(Weight_Log_Info)

## Data Exploration: Summarizing the statistical data

#Daily_Activity
Daily_Activity %>% 
  drop_na()%>% 
  select(TotalSteps, TotalDistance,
         VeryActiveMinutes, FairlyActiveMinutes, 
         LightlyActiveMinutes,
         SedentaryMinutes, Calories)%>%
  summary()

#Hourly_Calories
Hourly_Calories%>%
  drop_na()%>%
  select(Calories)%>%
  summary()

#Hourly_Intensities
Hourly_Intensities%>%
  drop_na()%>%
  select(TotalIntensity)%>%
  summary()

#Sleep_Day
Sleep_Day%>%
  drop_na()%>%
  select(TotalSleepRecords, 
         TotalMinutesAsleep, 
         TotalTimeInBed)%>%
  summary()

#Weight_Log_Info
Weight_Log_Info%>%
  drop_na()%>%
  select(WeightKg, BMI)%>%
  summary()

## Merging(Exploration)

Merged_Consumer_Data1 <- Reduce(function(
  Daily_Activity,Hourly_Intensities)
  merge(Daily_Activity,Hourly_Intensities, by='Id'),
  list(Hourly_Calories, Sleep_Day, Weight_Log_Info)
  )
Merged_Consumer_Data2 <-  merge(
  Daily_Activity,Hourly_Intensities, by='Id')

## Creating visualizations using ggplot2

#Relationship Between Calories and Total steps
ggplot(
  data = Daily_Activity,
  aes(x=TotalSteps, y=Calories))+
  geom_point()+ geom_smooth() + labs(
    title = "Relationship Between Calories and Total Steps")

#Average Calories Intake per Hour and Average Intensity per Hour

Hourly <- merge(
  Hourly_Calories, Hourly_Intensities, by =c('Id', 'ActivityHour', 'time', 'date')
)

HourlyAverage <- Hourly%>%
  group_by(time)%>%
  drop_na()%>%
  summarise(Average_Calories = mean(Calories), 
            Average_TotalIntensity = mean(TotalIntensity))

ggplot(
  data=HourlyAverage,
  aes(x=time, y=Average_Calories))+
  geom_point()+labs(
  title = "Average Calories intake per Hour")+
  theme(axis.text.x = element_text(angle = 90))

ggplot(
  data=HourlyAverage,
  aes(x=time, y=Average_Calories))+
  geom_histogram(stat = "identity", fill='purple')+
  labs(title = "Average Intensity intake per Hour")+
  theme(axis.text.x = element_text(angle = 90))
