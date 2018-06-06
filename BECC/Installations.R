setwd("~/Box Sync/ASES")
library(readxl)
X150120_Residential_Solar_Confidential_removed_1_20_15 <- read_excel("~/Box Sync/ASES/150120, Residential Solar Confidential removed 1-20-15.xls")
View(X150120_Residential_Solar_Confidential_removed_1_20_15)
Installations <- X150120_Residential_Solar_Confidential_removed_1_20_15
Installations$`Installed Zip` <- as.numeric(Installations$`Installed Zip`)
library(ggplot2)
library(scales)

#Cale says 
#download library lubridate 
#Build a snythetic data set where each line is a day 
#Add a "flag" to the data
#sum(Flag[date<= date] )
head(Installations)
Installations <- Installations[order(Installations$`App Received Date`),]

# Creating a new dataframe for companies 
# head(Installations$`App Received Date`)
Installers <- data.frame(Company=unique(Installations$Company))
# sapply(Installers$Company, function(x) min(Installations$`App Received Date`[Installations$Company==x]))
#Fail. Double checking. 
# sapply(Installers$Company, function(x) print(x))
# lapply(Installers$Company, function(x) (Installations$`App Received Date`[Installations$Company==x]))
# Installers$Company[85]
# Installations$`App Received Date`[Installations$Company=='AusTex Renewable Energy']
# Now our sapply command should work better. 
# sapply(Installers$Company, function(x) as.Date(min(Installations$`App Received Date`[Installations$Company==x])))
#JK, use lubridate. 
install.packages("lubridate")
library(lubridate)
Installations$AppDate <- as.Date(Installations$`App Received Date`)
head(Installations$AppDate)
sapply(Installers$Company, function(x) min(Installations$`AppDate`[Installations$Company==x], na.rm=TRUE))
sapply(Installers$Company, function(x) min(Installations$`AppDate`[Installations$Company==x]))
Installers$Company[1]
Installers$Company[96:97]
Installers$Company[97]
Installers$Company[96]

Installers <- Installers[2:95,]
sapply(Installers, function(x) min(Installations$`AppDate`[Installations$Company==x], na.rm=TRUE))
head(Installers)
Installers

Installers$FirstInstall <- sapply(Installers$Company, function(x) min(Installations$`AppDate`[Installations$Company==x], na.rm=TRUE))

Installers$FirstInstall <- as.Date(Installers$FirstInstall)

  #To merge two companies: 
##See what's in GREEN City
Installations$Company[Installations$Company=="GREEN CITY"]
##Change it
Installations$Company[Installations$Company=="GREEN CITY"] = "GREEN CITY SOLAR"
##See what's still there
Installations$Company[Installations$Company=="GREEN CITY"]

#To fill in blanks 
Installations$Company[is.na(Installations$Company)] = "Unknown"









#</Cale tutorial> 

#Maybe helpful? 
# http://neondataskills.org/R/time-series-convert-date-time-class-POSIX/

# Basics on Installations$`kW AC`
sum(Installations$`kW AC`, na.rm=TRUE)
mean(Installations$`kW AC`, na.rm=TRUE)
summary(Installations$`kW AC`)
length(Installations$`kW AC`)
hist(Installations$`kW AC`)
ggplot(Installations, aes(Installations$`kW AC`)) + geom_histogram() 
str(Installations)

# Basics on Installations$`Total Payment`
hist(Installations$`Total Payment`)

# On Installations$`Installed Zip`
barplot(table(Installations$`Installed Zip`), horiz = TRUE, las=2)
barplot(table(Installations$`Company`), horiz = TRUE, las=2)
table(Installations$`Company`)
hist(table(Installations$`Company`), breaks=25)
# Fail 
ggplot(Installations, aes(Installations$`Installed Zip`)) + geom_bar() 

hist(Installations$`Total Estimated Annual kWh Savings`, breaks=200)
summary(Installations$`Total Estimated Annual kWh Savings`)
plot(Installations$`Total Estimated Annual kWh Savings`~Installations$`App Received Date`)

# Trying breaks around years and months. No luck using breaks. 
hist(Installations$`App Received Date`, breaks="year")















ggplot(Installations, aes(`App Received Date`) + geom_freqpoly(mapping = NULL, data = NULL, stat = "bin", position = "identity", ..., na.rm = FALSE, show.legend = NA, inherit.aes = TRUE)
       

# Fail 
ggplot(Installations, aes(`Work Completed Date`,`kW AC`)) +
  geom_area() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  scale_x_datetime(labels = date_format("%Y"))

# Fail 
qplot(x=`Work Completed Date`, y=`kW AC`,
      data=Installations,
      main="Test", `Work Completed Date` >= as.POSIXct('2009-01-01 00:00',
                                          tz = "America/New_York") &
        `Work Completed Date` <= as.POSIXct('2011-12-31 23:59',
                               tz = "America/New_York"))

# Fail 
startTime <- as.Date("2010-01-01")
endTime <- as.Date("2016-01-01")
start.end <- c(startTime,endTime)
ggplot(Installations, aes(`Work Completed Date`,`kW AC`)) +
  geom_point(na.rm=TRUE, color="purple", size=1) + 
  ggtitle("Who knows") +
  xlab("Date") + ylab("kW AC")+ 
  (scale_x_date(limits=start.end,
                breaks=date_breaks("1 year"),
                labels=date_format("%b %y")))


