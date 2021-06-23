dir.create("data")
install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(tidyr)
library(tidyverse)
install.packages("gridExtra")
library(gridExtra)

#Data Representation
Fatalities_Data <- read.csv("data/bitrearddfatalitiesjanuary2018.csv", header = TRUE, na.strings=c("-9"))
class(Fatalities_Data)
summary(Fatalities_Data)
sapply(Fatalities_Data, mode)
sapply(df1, mode)
#Data Imputation
Speed.Limit = Fatalities_Data$Speed.Limit
Age = Fatalities_Data$Age
A <- is.na(Fatalities_Data$Age) # Identify records with NA values
Fatalities_Data$Age[A] <- mean(Fatalities_Data$Age, na.rm = TRUE)
S <- is.na(Fatalities_Data$Speed.Limit) # Identify records with NA values
Fatalities_Data$Speed.Limit[S] <- mean(Fatalities_Data$Speed.Limit, na.rm = TRUE)
#Data Cleaning
Fatalities_Filtered <- na.omit(Fatalities_Data)

Speed.Limit[is.na(Speed.Limit)] <- mean(Speed.Limit, na.rm = TRUE)
Age[is.na(Age)] <- mean(Age, na.rm = TRUE)

Bus..Involvement = Fatalities_Data$Bus..Involvement
Rigid.Truck..Involvement = Fatalities_Data$Rigid.Truck..Involvement
Articulated.Truck..Involvement. = Fatalities_Data$Articulated.Truck..Involvement.
Gender = Fatalities_Data$Gender

Fatalities_Filtered <- Fatalities_Data %>% filter(Bus..Involvement == -9) %>% filter(Rigid.Truck..Involvement == -9) %>% filter(Articulated.Truck..Involvement. == -9)%>% filter(Gender == -9)
Fatalities_Filtered
summary(Fatalities_Filtered)
?!is.na
#Exploratory Visualisation with ggplot
df1 <- data.frame(Crash.ID = Fatalities_Data$Crash.ID, Gender = Fatalities_Data$Gender, Year = Fatalities_Data$Year, CrashType = Fatalities_Data$Crash.Type, SpeedLimit = Fatalities_Data$Speed.Limit, Age = Fatalities_Data$Age, RoadUser = Fatalities_Data$Road.User, DayWeek = Fatalities_Data$Dayweek, Time = Fatalities_Data$Time, na.rm = TRUE)
Gender = Fatalities_Data$Gender
RoadUser = Fatalities_Data$Road.User
df1 %>% filter(!is.na(Gender)) %>% filter (!is.na(Age))
ggplot(data = subset(df1, !is.na(Gender)), mapping = aes(x = Gender, y = Age, fill = Gender)) + geom_boxplot()+ ggtitle("Road Fatalities - Age/Gender")
ggplot(data = subset(df1, !is.na(Gender)))+ geom_bar(mapping = aes(x = Age, fill = Gender), position = "dodge", width = 2) + ggtitle("Road Fatalities - Age/Gender")
ggplot(data = subset(df1, !is.na(RoadUser)))+ geom_bar(mapping = aes(x = SpeedLimit, fill = RoadUser), na.rm= TRUE, position = "dodge", width = 15) + xlim(0,120)+ theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("Road Fatalities - RoadUser/SpeedLimit")
ggplot(data = subset(df1, !is.na(CrashType)))+ geom_bar(mapping = aes(x = SpeedLimit, fill = CrashType), na.rm= TRUE, position = "dodge", width = 6) + xlim(0,120)+ theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("Road Fatalities - CrashType/SpeedLimit")

ggplot(data = subset(df1, !is.na(State)))+ geom_bar(mapping = aes(x = Year, fill = State), na.rm= TRUE, position = "dodge", width = 2) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("Road Fatalities - Year/State")
ggplot(data = subset(Fatalities_Data, !is.na(DayWeek)))+ geom_bar(mapping = aes(x = Month, fill = DayWeek), na.rm= TRUE, position = "dodge", width = .75) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("Road Fatalities - Month/DayWeek")

State = Fatalities_Data$State
ggplot(data = Fatalities_Data, geom_bar(mapping = aes(x = Year, fill = State), position = "fill", width = .25)) + theme_bw(base_size = 8) + ggtitle("Road Fatalities - SpeedLimit/CrashType")


ggplot(data = subset(df1, !is.na(CrashType)), mapping = aes(x = CrashType, y = SpeedLimit, fill = CrashType)) + geom_boxplot() + ylim(0,120) + ggtitle("Road Fatalities - SpeedLimit/CrashType")
ggplot(data = subset(df1, !is.na(Gender))) + geom_bar(mapping = aes(x = CrashType, fill = Gender), position = "fill")
RoadUser[df1$RoadUser == "n.a."] <- NA
Gender[df1$Gender == "n.a."] <- NA
df1 %>% filter(!is.na(Gender)) %>% filter (!is.na(RoadUser))
ggplot(data = subset(data, !is.na(Gender), !is.na(RoadUser))) + geom_bar(mapping = aes(x = RoadUser, fill = Gender), position = "fill", width = .25) + theme_bw(base_size = 8)   

ggplot(data = subset(df1, !is.na(Gender))) + geom_point(mapping = aes(x = RoadUser, y = Gender))     
df1 %>% filter(!is.na(Gender)) %>% filter (!is.na(RoadUser))%>%
  ggplot(data = df1) + geom_boxplot(mapping = aes(x = RoadUser, y = Gender))
Time = df1$Time
ggplot(data = df1) + geom_boxplot(mapping = aes(x= DayWeek, fill = DayWeek))                                  
ggplot(data = df1) + geom_point(mapping = aes(x= DayWeek, y = Time)) 
df1$Time <- format(df1$Time, format = "%H:%M")
df1$Time <- format(strptime(df1$Time,"%H:%M"), format = "%H:%M")
df1$Time1 = as.numeric(levels(df1$Time))[df1$time]

crashTime <- df1$Time
df1$Time <- as.character(df1$Time)
df1$crashTime <- sapply(strsplit(df1$Time,":"),function(x){ x<- as.numeric(x)
x[1]+x[2]/60})
df1$crashTime <- crashTime

summary(df1)
df1$Time <- (df1$Time = seq(as.POSIXlt("10:00", format = "%H:%M"), length = 100, by =10))

ggplot(data = df1, aes(x = DayWeek, y = crashTime, fill = DayWeek)) + geom_bar(stat ="identity") + scale_y_continuous(limits = c(0,24))
ggplot(df1, aes(x = DayWeek, y = time)) + geom_point()
ggplot(df1, aes(x = DayWeek, y = crashTime, fill = DayWeek)) + geom_boxplot() + scale_y_continuous(limits = c(0,24)) + ggtitle("Road Fatalities - DayWeek/CrashTime")

df1 %>% filter (!is.na(RoadUser))

ggplot(data = subset(df1, !is.na(RoadUser)), aes(x = RoadUser, y = SpeedLimit , fill = RoadUser)) + geom_boxplot() + ylim(1,120)+ theme(axis.text.x = element_text(angle = 60, hjust = 1)) + ggtitle("Road Fatalities - SpeedLimit/RoadUser")


df1$crashTime[10,]
ggplot(Fatalities_Data, aes(x = Year, fill = "Year")) + geom_bar() + ggtitle("Road Fatalities - Year")     

summary(df1)
ggplot(data = Year_Day_Time, mapping = aes(x = df1.DayWeek, y= df1.crashTime, colour = df1.DayWeek)) + geom_boxplot() + scale_y_continuous(limits = c(0,24)) + 
facet_grid(~df1.Year[,1:5])

DayWeek_1 <- data.frame()

WeekDay_Sat <- Year_Day_Time3 %>% filter(df1.DayWeek == "Saturday") %>% filter(crashTime == "Saturday")


CrashType = Fatalities_Data$CrashType
ggplot(data = Fatalities_Data, aes(x = Month, y = Year)) + geom_bar(stat= "Identity", na.rm = FALSE) + scale_y_continuous(limits= c(1989,2018)) 


Year <- as.numeric(df1$Year)
Year = df1$Year
DayWeek = df1$DayWeek
crashTime = df1$crashTime

Year_Day_Time2 <- Year_Day_Time %>% group_by(df1.Year, df1.DayWeek, df1.crashTime)
Year_Day_Time3 <- data.frame(df1$DayWeek, df1$crashTime, Age, Gender, na.rm = TRUE)
YDT_4 <- Year_Day_Time3 %>% group_by(df1.DayWeek, df1.crashTime)
YDT_5 <- (data = YDT_4)%>% summarise(df1.DayWeek~Sunday, mean(crashtime = df1.crashTime))
class(df1)
sapply(df1, mode)
typeof(Year)
Year = as.character(Year)
Year = as.numeric(Year)
Gender = as.character(DT_1$Gender)
Gender = as.numeric(DT_1$Gender)

DT_1 <- Year_Day_Time3 %>% group_by(df1.DayWeek, add = FALSE)
DT_2 <- read.csv("DT_1", header = TRUE)
DT_1$df1.crashTime = df1$df1.crashTime
DT_1 = DT_1
data(DT_1)
aggregate(DT_1[,2:3], list(DT_1$df1.DayWeek), mean, na.rm = TRUE)
mean(DT_1[["df1.crashTime"]])

data
DT_2
ggplot(Fatalities_Data, aes(x = DayWeek, fill = DayWeek)) + geom_bar() + ggtitle("Road Fatalities - DayWeek")

RStudio.Version()
sapply(DT_1, mode)
