############
# Attempt Number Three at getting a consoldiated EAE database together
# Project: ASES information
# Script Author: Mark Hand 
# Date: Sep2017
###########

############ Set wd, install packages, read in files ############
setwd("~/Box Sync/ASES/ASES_data")
library(readxl)
library(ggplot2)
library(scales)
library(lubridate)
library(data.table)
library(scales)
library(gridExtra)
library(grid)
library(cowplot)
library(tidyverse)
library(WriteXLS)
Installations <- read_excel("~/Box Sync/ASES/ASES_data/150120, Residential Solar Confidential removed 1-20-15.xls")
############# "There were 50 or more warnings (use warnings() to see the first 50)" (Why?) ########

############ Clean Installations data: Changing names, data types ############
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
Installations$Company[Installations$Company=="LIGHTHOUSE SOLAR"] = "Lighthouse Solar Austin"
Installations$Company[Installations$Company=="ARM/SOLAR COMMUNITY"] = "Solar Community"
Installations$Company[Installations$Company=="SOLAR COMMUNITY"] = "Solar Community"
Installations$Company[Installations$Company=="SOLERGY"] = "Austin Solergy"
Installations$Company[Installations$Company=="FREEDOM SOLAR"] = "Freedom Solar Power"
Installations$Company[Installations$Company=="LONGHORN"] = "Longhorn Solar"
Installations$Company[Installations$Company=="MERIDIAN"] = "Meridian Solar, Inc"
############ Consider merging Standard, Standard Renewable Energy and SRE? Two Solar Communities? Also both Enteros. But careful--figure out how to edit without messing up all the name changes below. ############

############ Loading and cleaning Weldon's list ############
Weldon <- read_excel("~/Box Sync/ASES/ASES_data/2017_08_17 Solar Austin fund raising list excerpt_solar.xlsx")
setnames(Weldon, old = c("Entity"), new = c("Company"))
setnames(Weldon, old = c("Email/Website"), new = c("Email"))
Weldon <- Weldon %>% unite("Contact", `Point of Contact`, X__1, sep = " ")
Weldon$Contact[Weldon$Contact=="NA NA"] = NA
Weldon$Contact <- gsub(x=Weldon$Contact, replacement = "", pattern = "NA")
Weldon$Company[Weldon$Company=="512 Solar"] = "512 Solar, LLC"
Weldon$Company[Weldon$Company=="Advanced Solar and Electric, LLC"] = "Advanced Solar Tech"
Weldon$Company[Weldon$Company=="Arroyo Electric"] = "Arroyo Electric & Solar"
Weldon$Company[Weldon$Company=="CAM Solar"] = "CAM Solar Inc"
Weldon$Company[Weldon$Company=="Circular Energy LP"] = "Circular Energy"
Weldon$Company[Weldon$Company=="CR Solar Energy Solutions"] = "CRsolar Energy Solutions"
Weldon$Company[Weldon$Company=="Custom Solar Electric"] = "CUSTOM SOLAR"
Weldon$Company[Weldon$Company=="Ecological Estates/Eco Estates International"] = "Ecological Estates LLC"
Weldon$Company[Weldon$Company=="Freedom Solar LLC"] = "Freedom Solar Power"
Weldon$Company[Weldon$Company=="Green Star Solutions"] = "Green Star Solutions Inc"
Weldon$Company[Weldon$Company=="Greenbelt Solar"] = "Greenbelt Solar Corp"
Weldon$Company[Weldon$Company=="HE Solar LLC"] = "HEsolar LLC"
Weldon$Company[Weldon$Company=="IES Residential"] = "ies residential"
Weldon$Company[Weldon$Company=="ImagineSolar"] = "IMAGINESOLAR"
Weldon$Company[Weldon$Company=="Lighthouse Solar"] = "LIGHTHOUSE SOLAR"
Weldon$Company[Weldon$Company=="Meridian Solar"] = "Meridian Solar, Inc"
Weldon$Company[Weldon$Company=="Solar Direct"] = "Solar Direct of Texas"
Weldon$Company[Weldon$Company=="Solar, Wind & Rain"] = "Solar Wind and Rain"
Weldon$Company[Weldon$Company=="TeraVolt Energy"] = "TeraVolt Energy Inc"
Weldon$Company[Weldon$Company=="Texas Electric"] = "TEXAS ELECTRIC"
Weldon$Company[Weldon$Company=="Texas Solar Power"] = "Texas Solar Power Company"
Weldon$Company[Weldon$Company=="Blue Heeler Electric"] = "Blue Paw Energy Service, LLC"
Weldon$Company[Weldon$Company=="LIGHTHOUSE SOLAR"] = "Lighthouse Solar Austin"
Weldon$Contact[1]="Mary Kelly"
Weldon$Email[1]="mary.kelly@bluepawenergy.com"
Weldon$Email[2]=NA
Weldon$Email[3]="jp.novak@512solar.com"
Weldon$Email[4]="Graeme@albaenergy.com"
Weldon$Contact[5]="Andrew Brannock"
Weldon$Email[6]="Kirk.wehby@ontility.com"
Weldon$Email[7]="info@pcisolar.com"
Weldon$Contact[9]="Cal Morton"
Weldon$Email[9]="c.morton@texassolaroutfitters.com"
Weldon$Contact[13]="Stan Pipkin"
Weldon$Email[13]="spipkin@lighthousesolar.com"
Weldon$Email[14]= "chris@dividendsolar.com (maybe)"
Weldon$Contact[14]="Christopher Doyle"
Weldon$Contact[15]="Xavier Mathabela"
Weldon$Email[15]="info@atxsolar.net"
Weldon$Contact[17]="Don Dickey"
Weldon$Email[17]="don@advancedsolar.com"
Weldon$Contact[18]="Gary Bynum"
Weldon$Email[18]="Garyb@greenstarsolutions.net"
Weldon$Contact[19]="Jesse Hinojosa"
Weldon$Email[19]="info@ SolarTEKEnergySA.com"
Weldon$Email[20]="Carl@WellsSolar.com"
Weldon$Email[21]="Jeff.wolfe@solaredge.com"
Weldon$Notes[22]="Magazine, not installer"
Weldon$Email[23]="michael.kuhn@imaginesolar.com"
Weldon$Email[24]="tim.padden@revolvesolar.com"
Weldon$Contact[25]="David Malone"
Weldon$Email[25]="david@1stchoiceenergy.com"
Weldon$Company[26]="7x Energy"
Weldon$Email[27]="jc@circularenergy.com"
Weldon$Email[28]="tpham@powerfinpartners.com"
Weldon$Contact[29]="Bret Biggart"
Weldon$Email[29]="bret@freedomsolarpower.com"
Weldon$Notes[30]="Manufacturer, not installer"
Weldon$Notes[31]="Research firm, not installer"
Weldon$Email[38]="hoss@tvnrg.com"
Weldon$Contact[38]="Hoss Boyd"
Weldon$Contact[45]="Andrew McCalla"
Weldon$Email[45]="andrew@meridiansolar.com"
Weldon$Contact[46]="Bob Bowne"
Weldon$Email[46]="bob@bowneelectric.com"
Weldon$Contact[52]="Jacqueline Hinman"
Weldon$Notes[52]="Not likely an installer"
Weldon$Email[52]="Jacqueline.hinman@ch2m.com"
Weldon$Email[56]="dpham@ecoestates.us"
Weldon$Contact[57]="Eric Hoffman"
Weldon$Email[57]="info@hesolarllc.com"
Weldon$Contact[58]="Christopher Renner"
Weldon$Contact[59]="Chris Kamykowski"  
Weldon$Email[60]="paul.bundschuh@idealpower.com"
Weldon$Contact[61]="Skeets Rapier"
Weldon$Email[61]="info@therenewablerepublic.com"
Weldon$Contact[64]="Carey Ibrahimbegovic"
Weldon$Email[64]="carey@greenbeltsolar.com"
Weldon$Contact[65]="Mitchell Tolbert"
Weldon$Contact[66]="Brian Cullen"
Weldon$Email[66]="brian@gocamsolar.com"
Weldon$Contact[71]="Jason Clark"  
Weldon$Contact[72]="Jason Clark"
Weldon$Notes[71]="Closed, acc. to Yelp"  
Weldon$Notes[73]="Website down"  
Weldon$Contact[73]="Blair Williamson"
Weldon$Email[73]="blair@williamson.net"
Weldon$Contact[74]="Neil Mcculloch"
Weldon$Notes[74]="Website down"    
Weldon$Contact[70]="Ken Janhs"
Weldon$Email[70]="admin@kdrservices.com"    
Weldon$Contact[69]="Emil Hristov"
Weldon <- Weldon[-c(33), ]
  
  ############ Creating a new dataframe for companies ############
Installers <- data.frame(Company=unique(Installations$Company))
Installers$FirstInstall <- sapply(Installers$Company, function(x) min(Installations$`AppDate`[Installations$Company==x], na.rm=TRUE))
Installers$FirstInstall <- as.Date(Installers$FirstInstall, origin = "1970-01-01")
Installers$FinalInstall <- sapply(Installers$Company, function(x) max(Installations$`AppDate`[Installations$Company==x], na.rm=TRUE))
Installers$FinalInstall <- as.Date(Installers$FinalInstall, origin = "1970-01-01")
Installers$LifeCycle <- as.numeric((Installers$FinalInstall-Installers$FirstInstall)/365)

############ Couldn't figure out how to join EINs database ############
EINs <- read_excel("~/Box Sync/ASES/ASES_data/AE_Installers.xlsx")
joined <-full_join(joined,EINs, by="Company")

############ Merging Installers and Weldon databases, and cleaning ############
joined <-full_join(Installers,Weldon)
joined$Contact[42]="Bryan Shellenberger"
joined$Notes[42]="Closed, acc to Yelp"
joined$Notes[41]="Closed, acc to Yelp"
joined$Contact[41]="Andrew Montney"
joined$Notes[41]="Linked to this company through LinkedIn https://www.bbb.org/denver/business-reviews/solar-energy-equipment-and-systems-dealers/altitude-marketing-inc-in-westminster-co-90151837"
joined$Email[41]="info@1800solarusa.net"
joined$Notes[40]="Might also be A-Apex, given connection to sketchy 1-800-solar-usa"
joined$Contact[29]="Shey Sabripour"
joined$Notes[29]="Closed, acc to Yelp"
joined$Contact[59]="Brandon Gibbs"
joined$Notes[59]="Closed. Gibbs now at 1 Sun Solutions"
joined$Contact[55]="Janet Hughes"
joined$Notes[55]="Website down; got Janet's name from LinkedIn"
joined$Contact[54]="John Paquin"
joined$Notes[54]="Website down; got John's name from LinkedIn"
joined$Contact[37]="Jim Wyatt"
joined$Email[37]="jim.wyatt@live.com"
joined$Notes[50]="All I could find: https://bizstanding.com/p/austin+solar+power+company-164989542"
joined$Contact[64]="Barry Garth"
joined$Notes[64]="Website down"
joined$Notes[60]="Closed, acc to Yelp"
joined$Contact[60]="Jim Sylvana"
joined$Notes[72]="AKA Efficient Air Conditioning & Electric"
joined$Contact[72]="George Drazic"
joined$Email[78]="info@efficienttexas.com"    
joined$Notesl[9]="Website down. The internet is a strange place https://www.youtube.com/watch?v=QOWrw2m8_5k"
joined$Contact[9]="Matt Spannaus"
joined$Contact[6]="Andrew McIndoo"
  joined$Contact[108]="Sean Runge"
  joined$Email[108]="srunge@cedgreentechtx.com"
  joined$Contact[40]="Andrew Montney"
  joined$Contact[25]="James Paxton"
  joined$Contact[28]="Shey Sabripour"
  joined$Notes[28]="Closed, acc to Yelp"
  joined$Notes[58]="Closed, it looks like"
  joined$Contact[58]="Charles Farmer"
  joined$Contact[71]="Jim Sweeney"
  joined$Contact[70]="Daniel Wang"
  joined$Contact[69]="John-Ross Cromer"
  joined$Notes[69]="Closed?"
  joined$Contact[68]="Jeff Benson"
  joined$Email[68]="info@enteroenergy.com"
  joined$Contact[63]="Jonathan Whittenberg"
  joined$Contact[21]="Rob McPherson"
  joined$Contact[34]="D. Martin Homes"
  joined$Contact[13]="Harry Klein"
  joined$Contact[17]="Steve Ellebracht"
  joined$Contact[30]="Rod Mueller"
  joined$Contact[24]="Linda Hwang"
  joined$Contact[35]="Adam Stepan"
  
# write_excel_csv(joined,"~/Box Sync/ASES/ASES_data/Working.xlsx",col_names = TRUE)
WriteXLS(joined, ExcelFileName = "Working.xls")
  
    
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
Cumulative_Installations
# A chart of Installations$kW_AC per month
Installations_permonth <- ggplot(Installations, aes(AppMonth, kW_AC)) + stat_summary(fun.y = sum) + geom_bar(stat='identity') + labs(list(title='Capacity installed per month', x='', y='kW'))
Installations_permonth
# chart of Installations$kw_AC per year 
Installations_peryear <- ggplot(Installations, aes(AppYear, kW_AC)) + stat_summary(fun.y = sum) + geom_bar(stat='identity') + labs(list(title='Capacity installed per year', x='', y='kW'))
Installations_peryear
# A chart of the lifecycle of companies 
Company_lifecycles <- ggplot(Installers, aes(LifeCycle)) + geom_histogram(binwidth=1) + coord_flip() + labs(list(title='Life cycle of firms in years', x='Years of life', y='Number of firms'))
Company_lifecycles
# + scale_x_discrete(breaks= pretty_breaks())

# Total revenue over time by company - but who is who? 
Revenuexcompany <- ggplot(Installations, aes(AppYear,Total_Payment)) + geom_bar(stat="identity", aes(fill=Company), show.legend=TRUE) + labs(list(title='Industry Revenue by company', x='', y='Total Revenue'))
Revenuexcompany

# Total installs over time by company - but who is who? 
Installsxcompany <- ggplot(Installations, aes(AppYear,kW_AC)) + geom_bar(stat="identity", aes(fill=Company), show.legend=FALSE) + labs(list(title='kW installed by company', x='', y='kW Installed'))
Installsxcompany

# Top 8 companies 
Companyinstalls <- as.data.frame(table(Installations$Company)) 
Revenuextop8companies <- ggplot(subset(Installations, Company %in% c("Native", "Freedom Solar Power", "Meridian Solar, Inc", "Revolve Solar", "Longhorn Solar", "Circular Energy", "Texas Solar Power Company", "Lighthouse Solar Austin")), aes(AppYear,Total_Payment)) + geom_bar(stat="identity", aes(fill=Company)) + labs(list(title='Industry Revenue - Top 8 Companies', x='', y='Total Revenue'))
Revenuextop8companies

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
