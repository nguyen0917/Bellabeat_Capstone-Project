library(tidyverse)
library(readr)
library(dplyr)
library(tibble)
library(psych)
library(ggplot2)
library(tidyverse)
library(RColorBrewer)
dailyActivity_merged <- read_csv("Downloads/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")

glimpse(dailyActivity_merged)

# obtain the # of missing data points per column
missing_values_count =  sum(is.na(dailyActivity_merged))
missing_values_count

# show basic information of data
str(dailyActivity_merged)

# count distinct value of "Id"
unique_id <- n_distinct(dailyActivity_merged$Id)
print(paste("Number of unique ID: ", unique_id))

# create a copy of the table to manipulate
# adding three columns: DayOfWeek, TotalExerciseMinutes, and TotalExerciseHours
# fill the columns with NaN
copy_daily <- dailyActivity_merged %>%
  add_column(DayOfWeek = NaN, .after = "ActivityDate") %>%
  add_column(TotalExcerciseMinutes = NaN, .after = "SedentaryMinutes") %>%
  add_column(TotalExcerciseHours = NaN, .after = "TotalExcerciseMinutes") 

# verify that the columns had been added successfully
str(copy_daily)

# Populate the values for the DayOfWeek column
copy_daily$DayOfWeek = weekdays(as.Date(dailyActivity_merged$ActivityDate, "%m/%d/%Y"))


# Populate the TotalExcerciseMinutes column by adding up all of the minutes of the corresponding day
copy_daily$TotalExcerciseMinutes = copy_daily$VeryActiveMinutes + copy_daily$FairlyActiveMinutes + copy_daily$LightlyActiveMinutes + copy_daily$SedentaryMinutes

# Populate the TotalExcerciseHours column by dividing the TotalExcerciseMinutes by 60
copy_daily$TotalExcerciseHours = round(copy_daily$TotalExcerciseMinutes/60, digits=0)
glimpse(copy_daily)

# getting the general statistics of the table
format(describeBy(copy_daily), scientific=FALSE)

# displaying the bar graph to see how often the users use the app throughout the week
ggplot(data=copy_daily, mapping= aes(x=DayOfWeek)) +
  geom_bar(color="black", fill="yellow" ) + 
  labs(title = "Number of time used throughout the week")

# extract the median calories and steps from above table
med_calories <-  2134
med_steps <- 7405

# scatter plot to graph Calories vs TotalSteps
# also plot the median calories and median steps
ggplot(data=copy_daily,mapping=aes(x=TotalSteps, y=Calories,color=Calories)) +
  geom_point() + scale_colour_gradient(low="red", high="green") +
  geom_hline(yintercept = med_calories)+ 
  geom_vline(xintercept = med_steps) +
  labs(title = "Calories burn per steps")+
  annotate("text",x=3850, y=5000,label="Median steps")+
  annotate("text",x=30000, y=2250,label="Median calories")
  


# extract the median hours and sedentary hours from above table
med_hours <- 24
med_sedentary <- 1057/60

# scatter plot to graph Calories vs TotalExcerciseHours
# also plot the median calories, median total hours, and median sedentary hour
ggplot(data=copy_daily,mapping=aes(x=TotalExcerciseHours, y=Calories,color=Calories)) +
  geom_point() + scale_colour_gradient(low="red", high="green") +
  geom_hline(yintercept = med_calories)+ 
  geom_vline(xintercept = med_hours) +
  geom_vline(xintercept = med_sedentary) +
  labs(title = "Calories burn per steps") +
  annotate("text",x=2, y=2250,label="Median calories")+
  annotate("text",x=15, y=0,label="Median sedentary")+
  annotate("text",x=22, y=5000,label="Median hours ")


# calculating total of individual minutes columns
very_active_mins = sum(copy_daily$VeryActiveMinutes)
fairly_active_mins = sum(copy_daily$FairlyActiveMinutes)
lightly_active_mins = sum(copy_daily$LightlyActiveMinutes)
sedentary_mins = sum(copy_daily$SedentaryMinutes)

# pie chart to display the minutes columns
value=c(very_active_mins,fairly_active_mins,lightly_active_mins ,sedentary_mins)
pie_labels <- paste0(round(100 * value/sum(value), 2), "%")
pie(value, labels = pie_labels,col=c("slateblue", "orange", "chartreuse4", "lightcoral"),main = "Activity in Minutes Percentage")

# legend of the pie chart 
legend("topleft", inset = -.0001, legend = c("Very Active","Fairly Active","Lightly Active" ,"Sedentary"), fill =  c("slateblue", "orange", "chartreuse4", "lightcoral"),cex = 0.85)
                                            


