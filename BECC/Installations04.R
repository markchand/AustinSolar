##### Readme #####
# Attempt Number Four at getting a consoldiated EAE database together
# Project: ASES information
# Script Author: Mark Hand 
# Date:  Oct 2017
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
library(ggthemes)

##### Load and clean Installations list #####
# "There were 50 or more warnings (use warnings() to see the first 50)" (Why?) 
Installations <- read_excel("~/Box Sync/ASES/ASES_data/150120, Residential Solar Confidential removed 1-20-15.xls")
Installations$AppDate <- as.Date(Installations$`App Received Date`)
Installations$AppYear <- as.Date(cut(Installations$AppDate,breaks = "year"))
Installations$AppMonth <- as.Date(cut(Installations$AppDate,breaks = "month"))
Installations$`Installed Zip` <- as.numeric(Installations$`Installed Zip`)
setnames(Installations, old = c('kW AC'), new = c('kW_AC'))
setnames(Installations, old = c('Total Payment'), new = c('Total_Payment'))
COs <- unique(Installations$Company)
COs <- sort(COs)
Installations$Company[Installations$Company=="U.S. Elementary"] = "US Elemental"
Installations$Company[Installations$Company=="U.S. ELEMENTAL"] = "US Elemental"
Installations$Company[Installations$Company=="US ELEMENTAL"] = "US Elemental"
Installations$Company[Installations$Company=="US ELEMENTARY"] = "US Elemental"
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
Installations$Company[Installations$Company=="Solar Works LLC DBA Solar Community LLC"] = "Solar Community"
Installations$Company[Installations$Company=="SRE"] = "Standard Renewable Energy"
Installations$Company[Installations$Company=="Stardard"] = "Standard Renewable Energy"
Installations$Company[Installations$Company=="ENTERO"] = "Entero Energy LLC"
Installations$Company[Installations$Company=="HILL COUNTRY ECOPOWER"] = "Native"
# Consider converting Global Energy > Solergy, based on shared phone numbers online. 

##### Loading and clean Weldon's list #####
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
Weldon$Company[Weldon$Company=="SolarCity"] = "TESLA"
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
  
##### Create a new dataframe for companies ############
Installers <- data.frame(Company=unique(Installations$Company))
Installers <- subset(Installers, Installers$Company!="test" & Installers$Company!="n/a")
lifecycle <- Installers 
lifecycle$FirstInstall <- sapply(lifecycle$Company, function(x) min(Installations$`AppDate`[Installations$Company==x], na.rm=TRUE))
# What error is that? 
lifecycle$FirstInstall <- as.Date(lifecycle$FirstInstall, origin = "1970-01-01")
lifecycle$FinalInstall <- sapply(lifecycle$Company, function(x) max(Installations$`AppDate`[Installations$Company==x], na.rm=TRUE))
# What error is that? 
lifecycle$FinalInstall <- as.Date(lifecycle$FinalInstall, origin = "1970-01-01")
lifecycle$LifeCycle <- as.numeric((lifecycle$FinalInstall-lifecycle$FirstInstall)/365)
# lifecycle <- lifecycle[!(lifecycle$Company=="Arroyo Electric & Solar" | lifecycle$Company=="test" | lifecycle$Company=="n/a"),]

##### Merge and clean Installers and Weldon databases ############
joined <-full_join(Installers,Weldon)
# What's that error? 
joined$Contact[joined$Company=="Pure Energy Solar Texas"]="Andrew McIndoo"
joined$Notesl[joined$Company=="electricians of texas llc"]="Website down. The internet is a strange place https://www.youtube.com/watch?v=QOWrw2m8_5k"
joined$Contact[joined$Company=="electricians of texas llc"]="Matt Spannaus"
joined$Contact[joined$Company=="Sorel Energy"]="Harry Klein"
joined$Contact[joined$Company=="Sky Blue Solar"]="Steve Ellebracht"
joined$Contact[joined$Company=="Standard Renewable Energy"]="Rob McPherson"
joined$Contact[joined$Company=="Solar Direct of Texas"]="Linda Hwang"
joined$Contact[joined$Company=="advanced power llc"]="James Paxton"
joined$Contact[joined$Company=="Altumaxis Technologies"]="Shey Sabripour"
joined$Contact[joined$Company=="CAM Solar Inc"]="Shey Sabripour"
joined$Notes[joined$Company=="Altumaxis Technologies"]="Closed, acc to Yelp"
joined$Notes[joined$Company=="CAM Solar Inc"]="Closed, acc to Yelp"
joined$Contact[joined$Company=="TE-Kyle LLC"]="Rod Mueller"
joined$Contact[joined$Company=="Green Solution Systems"]="D. Martin Homes"
joined$Contact[joined$Company=="Sunergy LLC"]="Adam Stepan"
joined$Contact[joined$Company=="AusTex Renewable Energy"]="Jim Wyatt"
joined$Email[joined$Company=="AusTex Renewable Energy"]="jim.wyatt@live.com"
joined$Contact[joined$Company=="AC Solar"]="Andrew Montney"
joined$Notes[joined$Company=="AC Solar"]="Might also be A-Apex, given connection to sketchy 1-800-solar-usa"
joined$Contact[joined$Company=="A-APEX Home Energy Management"]="Bryan Scellenberger"
joined$Notes[joined$Company=="A-APEX Home Energy Management"]="Linked to this company through LinkedIn https://www.bbb.org/denver/business-reviews/solar-energy-equipment-and-systems-dealers/altitude-marketing-inc-in-westminster-co-90151837"
joined$Email[joined$Company=="A-APEX Home Energy Management"]="info@1800solarusa.net"
joined$Notes[joined$Company=="A-APEX Home Energy Management"]="Closed, acc to Yelp"
joined$Notes[joined$Company=="Austin Solar Power Co."]="All I could find: https://bizstanding.com/p/austin+solar+power+company-164989542"
joined$Contact[joined$Company=="ARMADILLO"]="John Paquin"
joined$Notes[joined$Company=="ARMADILLO"]="Website down; got John's name from LinkedIn"
joined$Contact[joined$Company=="Janet's"]="Janet Hughes"
joined$Notes[joined$Company=="Janet's"]="Website down; got Janet's name from LinkedIn"
joined$Contact[joined$Company=="Solar Community"]="Brandon Gibbs / Shelby Ruff"
joined$Notes[joined$Company=="Solar Community"]="Closed. Gibbs now at 1 Sun Solutions"  
joined$Contact[joined$Company=="Radian"]="Charles Farmer"
joined$Notes[joined$Company=="Celestial"]="Closed, acc to Yelp"
joined$Contact[joined$Company=="Celestial"]="Jim Sylvana"
joined$Contact[joined$Company=="US Elemental"]="Jonathan Whittenberg"
joined$Contact[joined$Company=="Austin Solergy"]="Barry Garth"
joined$Notes[joined$Company=="Austin Solergy"]="Website down"
joined$Contact[joined$Company=="ENTERO"]="Jeff Benson"
joined$Email[joined$Company=="ENTERO"]="info@enteroenergy.com"
joined$Contact[joined$Company=="GREEN CITY SOLAR"]="John-Ross Cromer"
joined$Notes[joined$Company=="GREEN CITY SOLAR"]="Closed?"
joined$Contact[joined$Company=="Equinox"]="Daniel Wang"
joined$Contact[joined$Company=="GRUENE ENERGY"]="Jim Sweeney"
joined$Notes[joined$Company=="EFFICIENT ALTERNATIVE ENERGY"]="AKA Efficient Air Conditioning & Electric"
joined$Contact[joined$Company=="EFFICIENT ALTERNATIVE ENERGY"]="George Drazic"
joined$Email[joined$Company=="EFFICIENT ALTERNATIVE ENERGY"]="info@efficienttexas.com"    
joined$Contact[joined$Company=="Global Efficient Energy LLC"]="Abe Issa"
# NEW 
joined$Contact[joined$Company=="Texas Solar Power Company"]="Joe Garcia / Mark Selph"
joined$Contact[joined$Company=="Circular Energy"]="J Shore / Richard Estrada"
joined$Contact[joined$Company=="Freedom Solar Power"]="Bret Biggart / Ricardo Faraj"
joined$Contact[joined$Company=="ies residential"]="Brandon Ricket"
joined$Contact[joined$Company=="CAM Solar Inc"]="Shey Sabripour / Daniel Moyer"
joined$Contact[joined$Company=="Air Wind And Solar"]="Andrew Brannock / Amanda Eason / Daryl Eason"
joined$Contact[joined$Company=="Ontility"]="Kirk Wehby / Janet Hughes"
joined$Contact[joined$Company=="Alba Energy"]="Graeme Walker / Sarah Sanches"





removelist <- c("Infinergy Wind and Solar Central Texas", "Draker Energy", "Imagine Solar", "Altumaxis Technologies", "Younicos", "Solar Edge", "SolarPro", "7x Energy", "PowerFin Partners", "SunPower", "Virtus Energy", "SunModo", "CH2MHill", "SolarWorld", "Enphase", "Ideal Power Converters", "CED Greentech", "Crawford Electric Supply Co", "URS", "NA", "n/a", "test", "Draker Energy")

clean <- joined[ ! joined$Company %in% removelist, ]
  
# write_excel_csv(joined,"~/Box Sync/ASES/ASES_data/Working.xlsx",col_names = TRUE)
WriteXLS(clean, ExcelFileName = "Working.xls")
  
##### Fail to join EINs database ############
EINs <- read_excel("~/Box Sync/ASES/ASES_data/AE_Installers.xlsx")
joined <-full_join(joined,EINs, by="Company")
##### Some summary stuff ############
length(Installers$LifeCycle[Installers$LifeCycle>=0])
length(Installers$LifeCycle[Installers$LifeCycle>0])
summary(Installers$LifeCycle[Installers$LifeCycle>=0])
summary(Installers$LifeCycle[Installers$LifeCycle>0])
sd(Installers$LifeCycle[Installers$LifeCycle>=0])
sd(Installers$LifeCycle[Installers$LifeCycle>0])

##### Charts, Take 1 ############
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
Company_lifecycles <- ggplot(lifecycle, aes(LifeCycle)) + geom_histogram(binwidth=1) + coord_flip() + labs(list(title='Life cycle of firms in years', x='Years of life', y='Number of firms'))
Company_lifecycles
# + scale_x_discrete(breaks= pretty_breaks())

# Total revenue over time by company - but who is who? 
Revenuexcompany <- ggplot(Installations, aes(AppYear,Total_Payment)) + geom_bar(stat="identity", aes(fill=Company), show.legend=FALSE) + labs(list(title='Industry Revenue by company', x='', y='Total Revenue'))
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

library(ggplot2)
library(ggalt)
# clean$Company <- factor(clean$Company, levels=as.character(clean$Company))  
# This reorganizes the names in a way I don't understand. 
ggplot(data=subset(clean[clean$LifeCycle>0,]), 
       aes(x=FirstInstall, xend=FinalInstall, y=Company, group=Company)) +
  geom_dumbbell() +
  labs(x="Life Cycle", 
       title="The Life Cycle of Solar Panel Companies in Austin", 
       caption="Confidential - Check with mark.hand@utexas.edu before sharing")

##### Charts, Take 3 ####
# 1. kw installed / year 
Installsxcompany <- ggplot(Installations, aes(AppYear,kW_AC)) + geom_bar(stat="identity", aes(fill=Company), show.legend=FALSE) + labs(list(title='kW Installed (By Company)', x='', y='kW')) + scale_x_date(limits = as.Date(c('2004-01-01','2014-12-01')))

Installations_peryear <- ggplot(Installations, aes(AppYear, kW_AC)) + stat_summary(fun.y = sum) + geom_bar(stat='identity') + labs(list(title='Capacity installed per year', x='', y='kW')) + scale_x_date(limits = as.Date(c('2004-01-01','2014-01-01')))
``
# 2. Company lifecycles 
Company_lifecycles <- ggplot(lifecycle, aes(LifeCycle)) + geom_histogram() + labs(list(title='Life cycle of firms in years', x='Years of life', y='Number of firms'))
# + coord_flip()

# 3. Number of firms active each month/year
Installations %>% 
  filter(!is.na(AppMonth)) %>% 
  group_by(AppMonth) %>% 
  summarise(firms = n_distinct(Company)) %>% 
  select(AppMonth, firms) -> firmsxmonth

firmsxmonth.plot <- ggplot(firmsxmonth, aes(AppMonth, firms)) + geom_line(stat="identity") + scale_x_date(limits = as.Date(c('2004-01-01','2015-01-01')))

Installations %>% 
  filter(!is.na(AppYear)) %>% 
  group_by(AppYear) %>% 
  summarise(firms = n_distinct(Company)) %>% 
  select(AppYear, firms) -> firmsxyear

  firmsxyear.plot <- ggplot(firmsxyear, aes(AppYear, firms)) + geom_line(stat="identity") + scale_x_date(limits = as.Date(c('2004-01-01','2014-01-01'))) + labs(list(title='Number of active firms per year', x='', y='Number of  firms')) 

# 4  $/kw installed / year
Installations %>% 
  filter(!is.na(AppYear)) %>% 
  mutate(priceperkW = Total_Payment/kW_AC) %>% 
  select(AppYear, Total_Payment, kW_AC, priceperkW) %>% 
  group_by(AppYear) %>% 
  filter(priceperkW>0 & priceperkW<10000) %>% 
  mutate(meanpriceperkW = mean(priceperkW)) -> avgprice

avgprice.plot <- ggplot(avgprice, aes(AppYear, meanpriceperkW)) + geom_line(stat="identity") + scale_x_date(limits = as.Date(c('2004-01-01','2014-01-01'))) + labs(list(title='Averge price per kW', x='', y='$/kW')) 
  
# 5. Market share over time (FAIL) 
Installations$Company2 <- factor(Installations$Company, levels = c("Native", "Freedom Solar Power", "Meridian Solar, Inc", "Revolve Solar", "Longhorn Solar", "Circular Energy", "Texas Solar Power Company", "Lighthouse Solar Austin","Others"), labels = c("Native", "Freedom Solar Power", "Meridian Solar, Inc", "Revolve Solar", "Longhorn Solar", "Circular Energy", "Texas Solar Power Company", "Lighthouse Solar Austin","Others"))

ggplot(Installations, aes(AppYear,Total_Payment, group=Company2, fill=Company2)) + geom_area(stat="identity", show.legend=F)+ scale_x_date(limits = as.Date(c('2004-01-01','2013-01-01'))) + labs(list(title='Industry Revenue by company', x='', y='Total Revenue'))

# All together now 
grid.arrange(Installsxcompany, Company_lifecycles, firmsxyear.plot, avgprice.plot, nrow=2)

##### Next ############
# A chart of the number of installers active in a given month 
# Market share over time
# Installs per zip code over time (multiple maps)
# Installations (and revenue?) over time for each company 
# Measles chart - Installs per firm visualized with size of bubbles or depth of shading 
# Stacked Density Graph (FAIL) 
# http://www.r-graph-gallery.com/135-stacked-density-graph/
ggplot(Installations, aes(AppMonth, Group=Company)) + 
  geom_density(aes(fill=Company), position="stack", show.legend=F) + 
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
# Density plot (FAIL) 
# http://www.r-graph-gallery.com/21-distribution-plot-using-ggplot2
ggplot(Installations, aes(AppMonth, colour=Company, fill=Company)) + geom_density(alpha=0.55, show.legend=F)
