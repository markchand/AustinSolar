#### readme ##########################
# Author: mark.hand@utexas.edu       #
# Date: Dec 2017                     #
# Project: Exploring and exploiting  #
# in the Austin solar panel industry #
######################################

#### 1. Data and packages ####

# This is an anonymized version of the Austin Energy solar panel installation database. For the de-anonymized data, see PVH_Anon.R. 
# Packages used in this example: 
library(tidyverse)
library(plm) # For linear panel regression 
library(pglm) # For logistic panel regression 
library(gridExtra) # For multi-plot outputs 
library(stargazer) # For multi-model regression outputs 
# If you get an error, you may not have the packages installed.  Try running: install.packages("packagename") and/or update.packages().

# Setting working directory and importing data: 
setwd("~/Box Sync/ASES/ASES_data")
AE <- read_csv("~/Box Sync/ASES/ASES_data/Anonymized.csv")

#### 2. Creating variables #### 
# See below for variable rationale 
# Days_to_install - How long it takes to install something 
AE$Days_to_install <- as.numeric(as.Date(as.character(AE$`Final Inspection Date`))-as.Date(as.character(AE$`App Received Date`)))
# hist(AE$Days_to_install[AE$Days_to_install>0&AE$Days_to_install<365])
### NB: Need to fix this. Ask somebody. Also, 
# perwatt - Cost per watt of the system 
AE$perwatt <- as.numeric(AE$`Total Payment`/(AE$`kW AC`*1000))
# hist(AE$perwatt[AE$perwatt>0&AE$perwatt<10])
# Offness of estimate (%)
AE$offness <- as.numeric((AE$`Total Payment`-AE$`LOI Amount`)/AE$`LOI Amount`)
# Start year 
AE$year <- as.Date(cut(AE$`App Received Date`,breaks = "year"))
AE$year <- lubridate::year(AE$year)

# Getting rid of Inf values 
is.na(AE) <- do.call(cbind,lapply(AE, is.infinite))

#### 3. Building panel data ####
# Source: https://www.princeton.edu/~otorres/Panel101R.pdf

## Revenue panel(s)
panel <- AE %>% 
  group_by(Alias, year) %>% 
  summarise(
    number_of_installs = n(),
    revenue = sum(`Total Payment`),    
    avg_days_to_install = mean(Days_to_install, na.rm=T),
    sd_days_to_install = sd(Days_to_install, na.rm=T),
    avg_kW = mean(`kW AC`, na.rm=T),
    avg_perwatt = mean(perwatt, na.rm=T),
    sd_perwatt = sd(perwatt, na.rm=T),
    avg_offness = mean(offness, na.rm=T),
    sd_offness = sd(offness, na.rm=T)
    ) 

# Filtering out NAs
panel2 <- panel %>% 
  filter(!is.na(Alias)) %>% 
  filter(!is.na(year)) %>% 
  mutate(avg_perwatt=replace(avg_perwatt, which(avg_perwatt=="Inf"), NA)) %>% 
  mutate(avg_days_to_install=replace(avg_days_to_install, which(avg_days_to_install=="NaN"), NA)) %>% 
  mutate(avg_days_to_install=replace(avg_days_to_install, which(revenue==0), 0))

# Survival panels (incomplete)
AE %>% 
  group_by(Alias, year) %>% 
  summarise(
    number_of_installs = n())
# https://stats.stackexchange.com/questions/2134/r-package-for-fixed-effect-logistic-regression#2136
# https://www3.nd.edu/~rwilliam/stats3/Panel03-FixedEffects.pdf
# https://cran.r-project.org/web/packages/lme4/index.html
# https://cran.r-project.org/web/packages/pglm/pglm.pdf

## Market share 
industry <- panel2 %>% 
  group_by(year) %>% 
  summarise(
    industry_installs = sum(number_of_installs),
    industry_revenue = sum(revenue)
    )

panel3 <- left_join(panel2, industry, by = "year")

panel3 <- panel3 %>% 
  mutate(
    market_share = (revenue / industry_revenue)
  )

#### 4. Revenue Panels ####
RevModel1 <- plm(revenue ~ 
                     avg_days_to_install + 
                     avg_perwatt + 
                     avg_offness + 
                     sd_days_to_install + 
                     sd_perwatt + 
                     sd_offness, 
                   data = panel2)

RevModel2 <- plm(revenue ~ 
                     lag(avg_days_to_install) + 
                     lag(avg_perwatt) + 
                     lag(avg_offness) + 
                     lag(sd_days_to_install) + 
                     lag(sd_perwatt) + 
                     lag(sd_offness), 
                   data = panel2)

RevModel3 <- plm(revenue ~ 
                     lag(avg_days_to_install) + 
                     lag(avg_perwatt) + 
                     lag(avg_offness),
                   data = panel2) 

RevModel4 <- plm(revenue ~ 
                     lag(poly(avg_days_to_install, 2)) + 
                     lag(poly(avg_perwatt, 2)) + 
                     lag(poly(avg_offness, 2)),
                   data = panel2)

RevModel5 <- plm(revenue ~ 
                   lag(avg_days_to_install) + 
                   lag(avg_perwatt),
                 data = panel2) 

stargazer(RevModel2, RevModel3, RevModel4, RevModel5, type="text",
          covariate.labels=c("Avg time to completion","Avg per watt cost","Avg cost estimate error","Variation in time to completion","Variation in per watt cost","Variation in cost estimate error","Avg time to completion","Avg time to completion(squared)", "Avg per watt cost","Avg per watt cost(squared)",  "Avg cost estimate error",  "Avg cost estimate error(squared)"), 
          out="revmodels.htm"
          )

#### 5. Market Share Panels #### 

# testing
summary(plm(revenue ~
              lag(market_share),
            data=panel3))

ShareModel1 <- plm(market_share ~ 
                   avg_days_to_install + 
                   avg_perwatt + 
                   avg_offness + 
                   sd_days_to_install + 
                   sd_perwatt + 
                   sd_offness, 
                 data = panel3)

ShareModel2 <- plm(market_share ~ 
                   lag(avg_days_to_install) + 
                   lag(avg_perwatt) + 
                   lag(avg_offness) + 
                   lag(sd_days_to_install) + 
                   lag(sd_perwatt) + 
                   lag(sd_offness), 
                 data = panel3)

ShareModel3 <- plm(market_share ~ 
                     lag(avg_days_to_install) + 
                     lag(avg_perwatt) + 
                     lag(avg_offness),
                   data = panel3)

ShareModel4 <- plm(market_share ~ 
                   lag(poly(avg_days_to_install, 2)) + 
                   lag(poly(avg_perwatt, 2)) + 
                   lag(poly(avg_offness, 2)),
                 data = panel3)

ShareModel5 <- plm(market_share ~ 
                     lag(poly(avg_days_to_install, 2)) + 
                     lag(avg_perwatt) + 
                     lag(avg_offness),
                   data = panel3)

ShareModel6 <- plm(market_share ~ 
                     lag(poly(avg_days_to_install, 2)) + 
                     lag(avg_perwatt) + 
                     lag(avg_offness) + 
                     lag(sd_days_to_install) + 
                     lag(sd_perwatt) + 
                     lag(sd_offness), 
                   data = panel3)

ShareModel7 <- plm(market_share ~ 
                  lag(poly(avg_days_to_install, 2)) + 
                  lag(avg_perwatt), 
                data = panel3)

stargazer(ShareModel1, type="text")

stargazer(ShareModel2, ShareModel3, ShareModel4, ShareModel7, 
          type="text", 
          covariate.labels=c("Avg time to completion","Avg per watt cost","Avg cost estimate error","Variation in time to completion","Variation in per watt cost","Variation in cost estimate error","Avg time to completion","Avg time to completion(squared)", "Avg per watt cost","Avg per watt cost(squared)",  "Avg cost estimate error",  "Avg cost estimate error(squared)"), 
          out="sharemodels.htm")

#### 6. Survival Panels ####
## Other potential packages 
# https://cran.r-project.org/web/packages/survival/index.html
# https://cran.r-project.org/web/packages/survey/index.html
# https://cran.r-project.org/web/packages/lme4/index.html
# https://cran.r-project.org/web/packages/plm/index.html
# Making survival dataset 
grid <- expand.grid(year=2004:2015, Alias=unique(panel3$Alias))
panel4 <- full_join(panel3,grid) %>% 
  arrange(Alias, year) %>% 
  mutate(
    exists=ifelse(is.na(revenue),"No","Yes")
  )
# Creates a bunch of zeros, which allows pglm to run but (might) muck up the rest: 
# panel4[is.na(panel4)] <- 0

SurvivalModel1 <- pglm(exists ~ 
                         avg_days_to_install + 
                         avg_perwatt + 
                         avg_offness + 
                         sd_days_to_install + 
                         sd_perwatt + 
                         sd_offness, 
    data = panel4, family=binomial('probit'))

SurvivalModel2 <- pglm(exists ~ 
                         poly(avg_days_to_install, 2) + 
                         poly(avg_perwatt, 2) + 
                         poly(avg_offness, 2) + 
                         sd_days_to_install + 
                         sd_perwatt + 
                         sd_offness, 
                       data = panel4, family=binomial('probit'))

stargazer(SurvivalModel1)
# stargazer doesn't work with pglm, FML. 

#### Next Steps ####
# Or just use base R? https://cran.r-project.org/web/views/TimeSeries.html
# How to take average / later of two approval and inspection dates? 
# Use this for visualization? http://urbanspatialanalysis.com/dataviz-tutorial-mapping-san-francisco-home-prices-using-r/?utm_content=bufferbccb9&utm_medium=social&utm_source=twitter.com&utm_campaign=buffer

#### BELOW HERE BE DRAGONS #### 
#### Variable logic #### 
### Time to Completion 
# Start Date. On the front end, there is App Received Date, Aerial Site Assessment Date, and LOI Date. 
# A look at LOI Date suggests it regularly lags App Received Date, so we can toss that one out: 
ggplot(Installations, aes(`App Received Date`, `Aerial Site Assessment Date`)) + geom_point()
# And looking at App Received Date versus Aerial Site Assessment Date suggests that there maybe something wrong with the dates for Aerial Site Assessment: 
ggplot(Installations, aes(`App Received Date`, `Aerial Site Assessment Date`)) + geom_point()
# So: As a start date, we’ll use `App Received Date.`
# Not all observations have Work Complete Date. That seems to have started being recorded in 2011. Where observations do have Work Complete Date, the gap between that and the Final Inspection Date has a mean of __ and a standard deviation of __. 
ggplot(subset(AE['Work Completed Date'>2010]), aes(`Final Approval Date`, `Work Completed Date`)) + geom_point()
# So we’re stuck with Final Inspection Date and Final Approval Date. There’s a weird pattern about which of those come first: 
ggplot(AE, aes(`Final Approval Date`, `Final Inspection Date`)) + geom_point()
### per watt cost 
ggplot(AE, aes(`App Received Date`, `Total Payment`/`kW AC`)) + geom_point()

#### Plot play #### 
# Goal: Build plots (and tables?) of hypothesized variables. What am I trying to display? 

# random 
revenue <- ggplot(panel2) + geom_point(aes(year, revenue)) + geom_smooth(aes(year, revenue)) + theme_minimal()

avg_kW <- ggplot(panel2) + geom_point(aes(year, avg_kW)) + geom_smooth(aes(year, avg_kW)) + theme_minimal() + ylim(0,10)

revenue 

# stuff over time 
avg_days_to_install <- ggplot(panel2) + geom_point(aes(year, avg_days_to_install)) + geom_smooth(aes(year, avg_days_to_install)) + theme_minimal() + labs(x=NULL, y="Average time to completion (in days)", title="Figure 2: Visualizing changes in standardization variables over time")

sd_days_to_install <- ggplot(panel2) + geom_point(aes(year, sd_days_to_install)) + geom_smooth(aes(year, sd_days_to_install)) + theme_minimal() + ylim(0, 350) + labs(x=NULL, y="Variation in time to completion (sd)")

avg_perwatt <- ggplot(panel2) + geom_point(aes(year, avg_perwatt)) + geom_smooth(aes(year, avg_perwatt)) + theme_minimal() + labs(x=NULL, y="Average cost per watt (in dollars)")

sd_perwatt <- ggplot(panel2) + geom_point(aes(year, sd_perwatt)) + geom_smooth(aes(year, sd_perwatt)) + theme_minimal() + ylim(0,1.5) + labs(x=NULL, y="Variation in cost per watt (sd)")

avg_offness <- ggplot(panel2) + geom_point(aes(year, avg_offness)) + geom_smooth(aes(year, avg_offness)) + theme_minimal() + labs(x=NULL, y="Average estimate error (%)")

sd_offness <- ggplot(panel2) + geom_point(aes(year, sd_offness)) + geom_smooth(aes(year, sd_offness)) + theme_minimal() + ylim(0,1.5) + labs(x=NULL, y="Variation in estimate error (sd)") + ylim(0,.75)

# All the things over time 
grid.arrange(avg_days_to_install, sd_days_to_install, avg_perwatt, sd_perwatt, avg_offness, sd_offness, nrow=3)

# Area chart (#fail?) 
ggplot(panel2, aes(year,revenue,fill=Alias)) + geom_area(position="stack") + theme_minimal()

ggplot(panel2, aes(year,revenue,group=Alias,fill=Alias, colour=Alias)) + geom_bar(position="stack", stat="identity") + theme_minimal()

# Justifying quadratic variables
ggplot(panel2) + geom_point(aes(avg_perwatt, revenue)) + geom_smooth(aes(avg_perwatt, revenue)) + theme_minimal() + xlim(1,7) # wtf

ggplot(panel2) + geom_point(aes(avg_offness, revenue)) + geom_smooth(aes(avg_offness, revenue)) + theme_minimal() # How to bin into "other"? 

revenueVtime <- ggplot(panel2) + geom_point(aes(avg_days_to_install, revenue)) + geom_smooth(aes(avg_days_to_install, revenue)) + theme_minimal() + labs(x="Average time to completion (in days)", y="Firm revenue", title="Figure 1: Revenue v. Average time to completion") + ylim(0,200000)

revenueVinstalls 

#### Revisiting data for one installer ####
AE %>% 
  filter(Alias=="Bling Solar") %>% 
  arrange((Days_to_install)) -> Bling 

AE %>% 
  filter(Alias=="Flight Solar") %>% 
  group_by(year) %>% 
  summarise(
    revenue = sum(`Total Payment`),    
    #avg_days_to_install = mean(Days_to_install, na.rm=T),
    #sd_days_to_install = sd(Days_to_install, na.rm=T),
    avg_kW = mean(`kW AC`, na.rm=T),
    avg_perwatt = mean(perwatt, na.rm=T),
    sd_perwatt = sd(perwatt, na.rm=T),
    avg_offness = mean(offness, na.rm=T),
    sd_offness = sd(offness, na.rm=T))

#### Initial paneling #####
plm(revenue ~ avg_days_to_install, data = panel2)
# Makes sense. More time to install leads to less revenue. 

plm(revenue ~ number_of_installs, data = panel2)
# Makes sense. Each number is associated with a $11,000 increase in revenue. 

plm(revenue ~ lag(number_of_installs, 1), data = panel2)
plm(revenue ~ lag(number_of_installs, -1), data = panel2)
# Wait, what? 

plm(revenue ~ lag(avg_days_to_install,1), data = panel2)
# Makes sense; more time to install leads to less revenue.