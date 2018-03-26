#######################################
#Deloitte Assignment:                 
#Perform Exploratory Data Analysis on  
#2006 Pike's Peak 10k Race Analysis   
#Author: Jeff Gross                   
#Date: 3/7/2018                       
#######################################

#Libraries

library(readr)
library(tidyr)
library(stringr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(data.table)
library(reshape2)
library(stringi)

############
#LOAD DATA #
############

## Load Females Data Set

Females <- read_delim("MA_Exer_PikesPeak_Females.txt", 
                      "\t", escape_double = FALSE, col_types = cols(Pace = col_character()), 
                      trim_ws = TRUE)
Females$Gender = 0
Females$Gender_1 = 'Female'

## Load Males Data Set
Males <- read_delim("MA_Exer_PikesPeak_Males.txt", 
                    "\t", escape_double = FALSE, col_types = cols(`Gun Tim` = col_character(), 
                                                                  Pace = col_character()), trim_ws = TRUE)
Males$Gender = 1
Males$Gender_1 = 'Males'

# Combine both tables
Total <- rbind(Females, Males)

#############################################
# Exploratory Data Analysis: Combined Table #
#############################################

dim(Total)
head(Total)

#################
# Preprocessing #
#################

# Divide Div/Tot into two columns & convert Div_1 into a factor column & Create Division Column Based on PDF Specifications
Total_1 <- separate(Total, col = 'Div/Tot', into = c("Div", "Tot"), sep = "/")
Total_1$Div_1 <- as.factor(Total_1$Div)

#count Divisions
#count(Total_1, 'Div_1')

Total_1 %>%
  filter(Div == 1 & Gender==0)

Total_1 %>%
  filter(Div == 1 & Gender==1)

### Observation: According to the Dictionary provided on the PDF, "A division comprises racers of the same gender and
#age group". The divisions in this data set violate those parameters. Therefore, I am goiong to re-create the Divisions
#based on the parameters provided.

setDT(Total_1)[Ag <15 & Gender == 0, Division := 0]
Total_1[Ag <15 & Gender == 1, Division := 1]
Total_1[Ag >14 & Ag <20 &  Gender == 0, Division := 2]
Total_1[Ag >14 & Ag <20 & Gender == 1, Division := 3]
Total_1[Ag >19 & Ag <30 & Gender == 0, Division := 4]
Total_1[Ag >19 & Ag <30 & Gender == 1, Division := 5]
Total_1[Ag >29 & Ag <40 & Gender == 0, Division := 6]
Total_1[Ag >29 & Ag <40 & Gender == 1, Division := 7]
Total_1[Ag >39 & Ag <50 & Gender == 0, Division := 8]
Total_1[Ag >39 & Ag <50 & Gender == 1, Division := 9]
Total_1[Ag >49 & Ag <60 & Gender == 0, Division := 10]
Total_1[Ag >49 & Ag <60 & Gender == 1, Division := 11]
Total_1[Ag >59 & Ag <70 & Gender == 0, Division := 12]
Total_1[Ag >59 & Ag <70 & Gender == 1, Division := 13]
Total_1[Ag >69 & Ag <80 & Gender == 0, Division := 14]
Total_1[Ag >69 & Ag <80 & Gender == 1, Division := 15]
Total_1[Ag >79 & Gender == 0, Division := 16]
Total_1[Ag >79 & Gender == 1, Division := 17]

Total_1$Division <- as.factor(Total_1$Division)
levels(Total_1$Division)

# Examine Gun Tim(e) Column to Determine how to clean it up & convert it to a POSIXct type
#as.vector(Total_1$`Gun Tim`)

# Result: Need to remove D, A, M, V, N

Total_1$Gun_Time <- gsub("[A-Z]", "", Total_1$`Gun Tim`, fixed = TRUE)
Total_1$Gun_Time <- gsub("#", "", Total_1$`Gun_Time`, fixed = TRUE)
Total_1$Gun_Time <- gsub("D", "", Total_1$`Gun_Time`, fixed = TRUE)
Total_1$Gun_Time <- gsub("N", "", Total_1$`Gun_Time`, fixed = TRUE)
Total_1$Gun_Time <- gsub("M", "", Total_1$`Gun_Time`, fixed = TRUE)
Total_1$Gun_Time <- gsub("V", "", Total_1$`Gun_Time`, fixed = TRUE)
Total_1$Gun_Time <- gsub("A", "", Total_1$`Gun_Time`, fixed = TRUE)

# Need to remove spaces left from removing D, A, M, V, N
Total_1$Gun_Time <- gsub("[[:space:]]", "", Total_1$Gun_Time)

# Convert String data to POSIXct type given two forms of time MS, i.e '59:08', and HMS, i.e '1:02:14'
Total_1$Gun_Time <- parse_date_time(Total_1$Gun_Time, orders = c("MS", "HMS"))

#remove original Gun_Tim column
Total_1$Gun_Tim <- NULL

# Examine Net Tim(e) Column to Determine how to clean it up & convert it to a POSIXct type
#as.vector(Total_1$`Net Tim`)

# Result: Need to remove #, *
Total_1$Net_Time <- gsub("#", "", Total_1$`Net Tim`, fixed = TRUE)
Total_1$Net_Time <- gsub("*", "", Total_1$`Net_Time`, fixed = TRUE)
Total_1$Net_Time <- gsub("[A-Z]", "", Total_1$`Net_Time`, fixed = TRUE)

# Need to remove spaces left from removing #,*
Total_1$Net_Time <- gsub("[[:space:]]", "", Total_1$Net_Time)

# Convert String data to POSIXct type given two forms of time MS, i.e '59:08', and HMS, i.e '1:02:14'
Total_1$Net_Time <- parse_date_time(Total_1$Net_Time, orders = c("MS", "HMS"))

#remove original Net_Tim column
Total_1$Net_Tim <- NULL

# Examine Pace Column to Determine how to clean it up & convert it to a POSIXct type
#as.vector(Total_1$`Pace`)

# Result: Not necessary to clean up the 'Pace' column
# Convert String data to POSIXct type given only one form of time MS, i.e '59:08'
Total_1$Pace_1 <- parse_date_time(Total_1$Pace, orders = c("MS"))

#remove original Pace column
Total_1$Pace <- NULL

# Create New state Column
Total_1$State <- stri_sub(Total_1$Hometown, -2, -1)
Total_1$State <- gsub(" M", "MD", Total_1$State, fixed = TRUE)
Total_1$State <- gsub(" V", "VA", Total_1$State, fixed = TRUE)
count(Total_1, 'State')

# Convert Gender from character to factor type
Total_1$Gender <- as.factor(Total_1$Gender)

# Check for missing values
any(is.na(Total_1))

sum(is.na(Total_1$Div))
sum(is.na(Total_1$Tot))
sum(is.na(Total_1$Num))
sum(is.na(Total_1$Name))
sum(is.na(Total_1$Ag))
sum(is.na(Total_1$Hometown))
sum(is.na(Total_1$Gender))
sum(is.na(Total_1$Gun_Time))
sum(is.na(Total_1$Net_Time))
sum(is.na(Total_1$Pace_1))
sum(is.na(Total_1$State))
# Result: Total, Division, & Age contain missing values

###########################
#  Analysis for Questions #
###########################

#mean, median, & range by gender
Total_1 %>%
  dplyr::group_by(Gender_1) %>%
  dplyr::summarize(mean_Net_Time=mean(Net_Time), median_Net_Time=median(Net_Time), 
                   range_Net_Time=max(Net_Time) - min(Net_Time), 
                   min_Net_Time=min(Net_Time), max_Net_Time=max(Net_Time))

#filter Total_1 for only females
total_female <- Total_1 %>%
  group_by(Division, Gender_1, Place, Num, Ag, Pace_1, Net_Time) %>%
  filter(Gender == 0)

dim(total_female)

#mode for females
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

Mode(total_female$Net_Time)

#filter Total_1 for only males
total_male <- Total_1 %>%
  group_by(Division, Gender_1, Place, Num, Ag, Pace_1, Net_Time) %>%
  filter(Gender == 1)

dim(total_male)

head(total_male)

#mode for males
Mode <- function(x, na.rm = FALSE) {
  if(na.rm){
    x = x[!is.na(x)]
  }
  
  ux <- unique(x)
  return(ux[which.max(tabulate(match(x, ux)))])
}

Mode(total_male$Net_Time)

################################################################################################################
# Exercise Quesion #1: What are the mean, median, mode, and range of the race results for all racers by gender? #
################################################################################################################

#Answer:
  
#Females:
#       Mean: 58:29 (M:S)
#       Median: 57:51 (M:S)
#       Mode: 48:06 (M:S)
#       Range: 1.230833 hours

#Males:
#      Mean: 52:08 (M:S)
#      Median: 51:23 (M:S)
#      Mode: 35:28 (M:S)
#      Range: 1.197500 hours

# Observation: 

#On all measures Males were lower than females on their Net_Time. Further analysis would have to be done to 
#determine if it is significantly different

str(Total_1)

#Calculate difference between Gun_Time & Net_Time
Total_1$Diff <- Total_1$Gun_Time - Total_1$Net_Time

print("Mean:")
mean(Total_1$Diff)
print("Median:")
median(Total_1$Diff)
print("Range:(seconds)")
range(Total_1$Diff)

# Age Group

#Create new column for age_group
setDT(Total_1)[Ag <15, age_group := "0-14"]
Total_1[Ag >14 & Ag <20, age_group := "15-19"]
Total_1[Ag >19 & Ag <30, age_group := "20-29"]
Total_1[Ag >29 & Ag <40, age_group := "30-39"]
Total_1[Ag >39 & Ag <50, age_group := "40-49"]
Total_1[Ag >49 & Ag <60, age_group := "50-59"]
Total_1[Ag >59 & Ag <70, age_group := "60-69"]
Total_1[Ag >69 & Ag <80, age_group := "70-79"]
Total_1[Ag >79, age_group := "80+"]

#make age_group feature into factor type variable
Total_1$age_group <- as.factor(Total_1$age_group)

#remove NAs
Total_1_NA <- Total_1 %>% 
  filter(!is.na(Ag))

#Create plot of Diff by Gender
ggplot(Total_1_NA, aes(x = Diff, fill=Gender_1 )) +
  geom_histogram(binwidth=50, position = 'dodge') +
  ggtitle('Difference Between Gun & Net Time Grouped by Gender') +
  labs(x="Diff_Time (secs) (Gun_Time - Net_Time)", y="Total Count", fill = "Gender") +
  scale_fill_brewer(palette="Pastel1")

#Observation: At lower differences (0-100 sec) there are more males than females. At higher differences (>=200 sec),
#there are more females than males.
#Obervation: Bimodal distribution

ggplot(Total_1_NA, aes(x = Diff, fill=age_group )) +
  geom_histogram(binwidth=50, position = 'dodge') +
  ggtitle('Difference Between Gun & Net Time Grouped by Age Group') +
  labs(x="Difference Between Gun Time & Net Time (min:sec)", y="Total Count", fill = "Age Group") +
  scale_x_continuous(limits = c(0, 400), breaks=seq(0,400,50), 
                     labels=c('0', '0:50', '1:40','2:30', '3:20','4:10', '5:00','5:50', '6:40'))
#Observation: peak difference between Gun & Net Time with age group 30-39


scale_y_continuous(breaks=seq(1,10,1/4))
####################################################################################
#Exercise Question #2: Analyze the difference between gun and nettime race results.#
####################################################################################

#The difference between gun time and net time is small.:
  
#Mean: 171 secs
#Median: 196 secs
#Range: 560 seconds

#Within this range, the histogram of the total difference and by gender indicates a bi-modal distribution
#peak difference between Gun & Net Time with age group 30-39
#At lower differences (0-100 sec) there are more males than females. At higher differences (>=200 sec),
# there are more females than males.

###########################################################################################################
# Question #3: How much time separates Chris Doe fromthe top 10 percentile of racers of the samedivision? #
###########################################################################################################

#I did not use the Division column provided in the date set as FAQ documents states "Follow the directions regarding 
#divisioning, rather than the (not very accurate) division information in the data. "

#Determine Chris Doe's division
Total_1 %>%
  filter(Name == 'Chris Doe')
#result Chris Doe is in division 9

#calculate Net_Time for Chris Doe 
Total_Chris <- Total_1 %>% group_by(Division) %>% 
  filter(Name == 'Chris Doe')

Total_Chris$Net_Time
#"0000-01-01 00:49:43 UTC"

Total_9 <- Total_1 %>% 
  filter(Division==9)

dim(Total_9)

#Calculate top 10 percentile of racers of the same division
Total_10 <- Total_1 %>% 
  filter(Division==9) %>%
  filter(quantile(Net_Time, 0.1)>Net_Time)

range(Total_10$Net_Time)
#"0000-01-01 00:33:06 UTC" "0000-01-01 00:41:39 UTC"

#max top 10%
Total_10_max <- max(Total_10$Net_Time)
Total_10_max

Total_50 <- Total_1 %>% 
  filter(Division==9) %>%
  filter(quantile(Net_Time, 0.5)>Net_Time)

range(Total_50$Net_Time)
# "0000-01-01 00:33:06 UTC" "0000-01-01 00:50:41 UTC"

Total_40 <- Total_1 %>% 
  filter(Division==9) %>%
  filter(quantile(Net_Time, 0.4)>Net_Time)

range(Total_40$Net_Time)
#"0000-01-01 00:33:06 UTC" "0000-01-01 00:48:43 UTC"

#Chris Does with a Net Time of #"0000-01-01 00:49:43 UTC" was in the 50% percentile

mean(Total_9$Net_Time)

range(Total_9$Net_Time)

#Calculate how much time separates Chris Doe fromthe top 10 percentile of racers of the same division
Total_Chris$Net_Time - Total_10_max

#Answer based on Divisions based on rules in PDF: Time difference of 8.066667 mins

ggplot() +
  geom_point(data=Total_9, aes(x=Num, y=Net_Time)) +  
  geom_point(data=Total_10, aes(x=Num, y=Net_Time, color='blue')) +
  geom_point(data=Total_Chris, aes(x=Num, y=Net_Time, color='red', size=20)) +
  labs(x="Race Number", y="Net Time (min:sec)", color = "Color Coding") +
  ggtitle("Net Time versus Race Number for Division Nine") +
  scale_color_manual(labels = c("Top 10%", "Chris Doe"), values = c("blue", "red")) +
  scale_size(guide=FALSE)

#########################################################################
# Question 4: Compare the race results of each division with divisions  #
#########################################################################

#This question was answered based on PDF rules

#make Place feature into factor type variable
#Total_1$Place_1 <- as.factor(Total_1_NA$Place)

#find unique levels for Place_1 feature
#levels(Total_1$Place_1)
# Observation: Two many levels in Place column to Facet

#Create bar plot of Total Count vs Division
ggplot(data=Total_1_NA ,aes(x=Division)) + 
  geom_bar(aes( fill=Gender_1)) + 
  geom_text(stat='count', binwidth=1,aes(label=..count..),vjust=-1) +
  labs(x="Division", y="Total Count", fill = "Gender") +
  ggtitle("Total Count versus Division") +
  scale_fill_brewer(palette="Pastel1")

#Create bar plot of Total Count vs Age Group
ggplot(data=Total_1_NA ,aes(x=age_group)) + 
  geom_bar(aes( fill=Gender_1)) + 
  geom_text(stat='count', binwidth=1,aes(label=..count..),vjust=-1) +
  labs(x="Age Group", y="Total Count", fill = "Gender") +
  ggtitle("Total Count versus Age Group") +
  scale_fill_brewer(palette="Pastel1")

#Observation: # of runners increased from Age Group 0-14 until a peak between Age group 20 & 39, then decreased 
#Observation: The men outnumbered the female for every age group except two, 20-29 & 30-39

#Create box plot of Net Time vs Division by Gender
ggplot(Total_1_NA, aes(x = Division, y = Net_Time, fill = Gender_1)) +
  geom_boxplot(width = 1) +
  ggtitle("Net Time versus Division") +
  labs(x="Division", y="Net Time (H:M)", fill = "Gender") +
  scale_fill_brewer(palette="Pastel1")

#Create box plot of Net Time vs Age Group by Gender
ggplot(Total_1_NA, aes(x = age_group, y = Net_Time, fill = Gender_1)) +
  geom_boxplot(width = 1) +
  ggtitle("Net Time versus Age Group") +
  labs(x="Age Group", y="Net Time (H:M)", fill = "Gender") +
  scale_fill_brewer(palette="Pastel1")

#Observation: Net_Time higher for females vs males for all divisions except Age 60-69 (Division 12 and 13)

#Create box plot of Net Time vs Division by Age Groups
ggplot(Total_1_NA, aes(x = Division, y = Net_Time, fill = age_group)) +
  geom_boxplot(width = 1) +
  ggtitle("Net Time versus Division") +
  labs(x="Division", y="Net Time (H:M)", fill = "Age Groups")

#Observation: Downward trend of Net Time from 0-19 & then a slight upward trend from 20-69 and a peak at 70+


