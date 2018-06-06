############
# Trying to recreate and codify what Cale and I worked on 
# Project: ASES information
# Script Author: Mark Hand 
# Date: 06-22-2017
###########

############ Set wd, install packages, read in files ############
setwd("~/Box Sync/ASES")
library(readxl)
library(ggplot2)
library(scales)
library(lubridate)
library(data.table)
library(scales)
library(gridExtra)
library(grid)
library(cowplot)
Installations <- read_excel("~/Box Sync/ASES/150120, Residential Solar Confidential removed 1-20-15.xls")
############# "There were 50 or more warnings (use warnings() to see the first 50)" (Why?) ########

############ Clean data: Changing names, data types ############
Installations$AppDate <- as.Date(Installations$`App Received Date`)
Installations$AppYear <- as.Date(cut(Installations$AppDate,breaks = "year"))
Installations$AppMonth <- as.Date(cut(Installations$AppDate,breaks = "month"))
Installations$`Installed Zip` <- as.numeric(Installations$`Installed Zip`)
setnames(Installations, old = c('kW AC'), new = c('kW_AC'))
setnames(Installations, old = c('Total Payment'), new = c('Total_Payment'))
Installations$Company[Installations$Company=="U.S. Elementary"] = "US Elemental"
Installations$Company[Installations$Company=="U.S. ELEMENTAL"] = "US Elemental"
Installations$Company[Installations$Company=="US ELEMENTAL"] = "US Elemental"
Installations$Company[Installations$Company=="TXSPC"] = "Texas Solar Power Company"
Installations$Company[Installations$Company=="TXSPC-1 HOUSE"] = "Texas Solar Power Company"
Installations$Company[Installations$Company=="TSPC"] = "Texas Solar Power Company"
Installations$Company[Installations$Company=="Solar Direct of Texas, LLC"] = "Solar Direct of Texas"
Installations$Company[Installations$Company=="SELF RELIANT"] = "Self Reliant Solar"
Installations$Company[Installations$Company=="SELF RELIANT SOLAR"] = "Self Reliant Solar"
Installations$Company[Installations$Company=="NEWPOINT"] = "SRE"
Installations$Company[Installations$Company=="NEWPOINT (SRE)"] = "SRE"
Installations$Company[Installations$Company=="GREEN CITY"] = "GREEN CITY SOLAR"
Installations$Company[Installations$Company=="CR SOLAR"] = "CRsolar Energy Solutions"
Installations$Company[Installations$Company=="GLOBAL"] = "Global Energy"
Installations$Company[Installations$Company=="E.E. International LLC DBA EcoOne Homes"] = "Ecological Estates LLC"
Installations$Company[Installations$Company=="CIRCULAR"] = "Circular Energy"
Installations$Company[Installations$Company=="CELESTIAL POWER"] = "Celestial"
Installations$Company[Installations$Company=="AUSTIN SPC"] = "Austin Solar Power Co."
Installations$Company[Installations$Company=="Advanced Solar and Electric LLC"] = "Advanced Solar Tech"

############ Creating a new dataframe for companies ############
Installers <- data.frame(Company=unique(Installations$Company))
Installers$FirstInstall <- sapply(Installers$Company, function(x) min(Installations$`AppDate`[Installations$Company==x], na.rm=TRUE))
Installers$FirstInstall <- as.Date(Installers$FirstInstall, origin = "1970-01-01")
Installers$FinalInstall <- sapply(Installers$Company, function(x) max(Installations$`AppDate`[Installations$Company==x], na.rm=TRUE))
Installers$FinalInstall <- as.Date(Installers$FinalInstall, origin = "1970-01-01")
Installers$LifeCycle <- as.numeric((Installers$FinalInstall-Installers$FirstInstall)/365)

############ Some summary stuff ############
length(Installers$LifeCycle[Installers$LifeCycle>=0])
length(Installers$LifeCycle[Installers$LifeCycle>0])
summary(Installers$LifeCycle[Installers$LifeCycle>=0])
summary(Installers$LifeCycle[Installers$LifeCycle>0])
sd(Installers$LifeCycle[Installers$LifeCycle>=0])
sd(Installers$LifeCycle[Installers$LifeCycle>0])

############ Charts ############
# A chart of Installations$kW_AC over Installations$AppDate (cumulative)
Cumulative_Installations <- ggplot(Installations, aes(x=AppDate, y=cumsum(kW_AC))) + geom_step() + labs(list(title='Cumulative installed capacity', x='',y='kW'))
# A chart of Installations$kW_AC per month
Installations_permonth <- ggplot(Installations, aes(AppMonth, kW_AC)) + stat_summary(fun.y = sum) + geom_bar(stat='identity') + labs(list(title='Capacity installed per month', x='', y='kW'))
# chart of Installations$kw_AC per year 
Installations_peryear <- ggplot(Installations, aes(AppYear, kW_AC)) + stat_summary(fun.y = sum) + geom_bar(stat='identity') + labs(list(title='Capacity installed per year', x='', y='kW'))
# A chart of the lifecycle of companies 
Company_lifecycles <- ggplot(Installers, aes(LifeCycle)) + geom_histogram(binwidth=1) + coord_flip() + labs(list(title='Life cycle of firms in years', x='Years of life', y='Number of firms'))
# + scale_x_discrete(breaks= pretty_breaks())

# Total revenue over time by company - but who is who? 
Revenuexcompany <- ggplot(Installations, aes(AppYear,Total_Payment)) + geom_bar(stat="identity", aes(fill=Company), show.legend=FALSE) + labs(list(title='Industry Revenue by company', x='', y='Total Revenue'))

# Total installs over time by company - but who is who? 
Installsxcompany <- ggplot(Installations, aes(AppYear,kW_AC)) + geom_bar(stat="identity", aes(fill=Company), show.legend=FALSE) + labs(list(title='kW installed by company', x='', y='kW Installed'))

# Top 8 companies 
Companyinstalls <- as.data.frame(table(Installations$Company)) 
Revenuextop8companies <- ggplot(subset(Installations, Company %in% c("Native", "Freedom Solar Power", "MERIDIAN", "Revolve Solar", "Longhorn Solar", "Circular Energy", "Texas Solar Power Company", "Lighthouse Solar Austin")), aes(AppYear,Total_Payment)) + geom_bar(stat="identity", aes(fill=Company)) + labs(list(title='Industry Revenue - Top 8 Companies', x='', y='Total Revenue'))

# Plotting 
plot_grid(Cumulative_Installations, Installations_permonth, Installations_peryear, Company_lifecycles, align='h')

plot_grid(Revenuexcompany, Installsxcompany, Revenuextop8companies, align='h')

# Failed but still interesting geom_raster
ggplot(Installations, aes(AppMonth, Company)) + geom_raster(aes(fill = length(AppDate)), show.legend=FALSE)

############ Next ############
# A chart of the number of installers active in a given month 
# Market share over time
# Installs per zip code over time (multiple maps)
# Installations (and revenue?) over time for each company 
# Measles chart - Installs per firm visualized with size of bubbles or depth of shading 
############ Next ############

############
# Junk
############

# ggplot(Installations, aes(AppYear,Total_Payment)) + geom_area(stat="identity", position="stack", aes(fill=Company), show.legend=FALSE) + labs(list(title='Industry Revenue by company', x='', y='Total Revenue'))

#ggplot(subset(Installations,Company %in% c("Native")), aes(x=AppYear, y=Total_Payment), group=Company, col=Company) + geom_histogram(stat = "identity")

#ggplot(subset(Installations,Company %in% c("Native", "Texas Solar Power Company")), aes(x=AppYear, y=Total_Payment, group=Company, col=Company)) + geom_count()

# ggplot(subset(Installations,Company %in% c("Native"))) +
#  stat_summary(fun.y = sum, geom="point", colour = "red", size = 1)

# ggplot(Installations, aes(AppMonth, Total_Payment), group=Company, col=Company) + stat_summary(fun.y = sum) + geom_count() +coord_cartesian(ylim=c(0,100000)) + facet_wrap(~Company) 
#+ labs(list(title='Capacity installed per month', x='', y='kW'))
# https://stackoverflow.com/questions/18165578/subset-and-ggplot2

# To view the data in order (why not just click the button, Cale?)
# Installations <- Installations[order(Installations$`App Received Date`),]
# Create dates we can work with 

# Cale says build a snythetic data set where each line is a day, Add a "flag" to the data 
#sum(Flag[date<= date])

# sapply(Installers$Company, function(x) min(Installations$`App Received Date`[Installations$Company==x]))
#Fail. Double checking. 
# sapply(Installers$Company, function(x) print(x))
# lapply(Installers$Company, function(x) (Installations$`App Received Date`[Installations$Company==x]))
# Installers$Company[85]
# Installations$`App Received Date`[Installations$Company=='AusTex Renewable Energy']
# Now our sapply command should work better. 
# sapply(Installers$Company, function(x) as.Date(min(Installations$`App Received Date`[Installations$Company==x])))
#JK, use lubridate. 

# sapply(Installers$Company, function(x) min(Installations$`AppDate`[Installations$Company==x], na.rm=TRUE))
# sapply(Installers$Company, function(x) min(Installations$`AppDate`[Installations$Company==x]))
# Installers$Company[1]
# Installers$Company[96:97]
# Installers$Company[97]
# Installers$Company[96]
# Installers <- Installers[2:95,]
# sapply(Installers, function(x) min(Installations$`AppDate`[Installations$Company==x], na.rm=TRUE))

#To fill in blanks 
# Installations$Company[is.na(Installations$Company)] = "Unknown"

# To merge two companies: 
# See what's in GREEN City
# Installations$Company[Installations$Company=="GREEN CITY"]
# Change it
# Installations$Company[Installations$Company=="GREEN CITY"] = "GREEN CITY SOLAR"
# See what's still there
# Installations$Company[Installations$Company=="GREEN CITY"]

############
# </Junk> 
############
