#### Setup #####
# This is an anonymized version of the Austin Energy solar panel installation database. For the de-anonymized data, see PVH_Anon.R. 
# Packages used in this example: 
library(tidyverse)
theme_set(theme_minimal()) # Sets default visual theme for plots 
library(plm) # For linear panel regression 
library(pglm) # For logistic panel regression 
library(gridExtra) # For multi-plot outputs 
library(stargazer) # For multi-model regression outputs 
library(foreign) # For scatterplots. Probably not necessary
library(scales) # For rearranging axes 
library(lubridate) # For dealing with dates, maybe
# If you get an error, you may not have the packages installed.  Try running: install.packages("packagename") and/or update.packages().

# Setting working directory and importing data: 
setwd("~/Box Sync/ASES/ASES_data/PVH")
AE <- read_csv("~/Box Sync/ASES/ASES_data/PVH/Anonymized.csv")


#### Wrangling ####
# Getting rid of Inf values 
is.na(AE) <- do.call(cbind,lapply(AE, is.infinite))

# Days to install 
AE$Days_to_install <- as.numeric(as.Date(as.character(AE$`Final Inspection Date`))-as.Date(as.character(AE$`App Received Date`)))

# Adding variables and filtering out the crazies 
AE %>% 
  mutate(
    year = year(`App Received Date`),
    inefficiency = Days_to_install/AE$`kW AC`,
    perwatt = as.numeric(`Total Payment`/(`kW AC`*1000)),
    offness = as.numeric(((AE$`Total Payment`-AE$`LOI Amount`)/AE$`LOI Amount`)*100)
  ) %>% 
  filter(
    year>2001 & 
      year<2015 &
      Days_to_install<351 &
      Days_to_install>0 &
      offness<200 &
      perwatt>0 &
      perwatt<10 &
      `kW AC` >0 &
      `kW AC` < 50
       ) %>% 
  group_by(year) %>% 
  mutate(
    pwsby=((perwatt-mean(perwatt))/sd(perwatt)),
    industry_installs = n(),
    industry_revenue = sum(`Total Payment`)) -> AE1

# Adding market share variable 
AE1 %>% 
  group_by(Alias, year) %>% 
  mutate(
    revenue = sum(`Total Payment`),
    market_share = revenue/industry_revenue) -> AE1
    
# summarizing 
AE1 %>% 
  summarise(
    number_of_installs = n(),
    revenue = sum(`Total Payment`),
    market_share = mean(market_share),
    avg_inefficiency = mean(inefficiency, na.rm=T),
    cv_inefficiency = sd(inefficiency, na.rm=T)/mean(inefficiency, na.rm=T),
    avg_kW = mean(`kW AC`, na.rm=T),
    avg_pwsby = mean(pwsby, na.rm=T),
    cv_pwsby = sd(pwsby, na.rm=T)/mean(pwsby, na.rm=T),
    avg_offness = mean(offness, na.rm=T),
    cv_offness = sd(offness, na.rm=T)/mean(offness, na.rm=T)) -> AEsmall

AEsmall %>% 
  filter(!is.na(Alias)) %>% 
  mutate(
    cv_offness=replace(cv_offness, which(cv_offness=="NaN"), NA),
    cv_offness=replace(cv_offness, which(cv_offness>5000), NA)
         ) -> AEsmall

AEsmall %>% 
  mutate(cv_inefficiency=replace(cv_inefficiency, which(is.na(cv_inefficiency)),0),
         cv_pwsby=replace(cv_pwsby, which(is.na(cv_pwsby)),0),
         cv_offness=replace(cv_offness, which(is.na(cv_offness)),0)
         ) -> AEpoly

#### Distribution of variables ####
# Variables used in regression 
revenue_histogram <- ggplot(AEsmall, aes(revenue)) + geom_histogram(fill="#00A4E0") + labs(title="Revenue per firm per year", x="Revenue", y="# of firm-years")
market_share_histogram <- ggplot(AEsmall, aes(market_share)) + geom_histogram(fill="#00A4E0")+ labs(title="Market share per firm per year", x="Revenue", y="")

avg_inefficiency_histogram <- ggplot(AEsmall, aes(avg_inefficiency)) + geom_histogram() + labs(title="Mean Time to completion", x="Days/kW", y="# of firm-years")
cv_inefficiency_histogram <- ggplot(AEsmall, aes(cv_inefficiency)) + geom_histogram() + labs(title="Variation in time to completion", x="Coefficient of variation", y="")

avg_pwsby_histogram <- ggplot(AEsmall, aes(avg_pwsby)) + geom_histogram() + labs(title="Mean per watt cost", x="", y="# of firm-years")
cv_pwsby_histogram <- ggplot(AEsmall, aes(cv_pwsby)) + geom_histogram() + labs(title="Variation in per watt cost", x="Coefficient of variation", y="")

avg_offness_histogram <- ggplot(AEsmall, aes(avg_offness)) + geom_histogram() + labs(title="Mean cost estimate error", x="Percent off", y="# of firm-years")
cv_offness_histogram <- ggplot(AEsmall, aes(cv_offness)) + geom_histogram() + labs(title="Variation in cost estimate error", x="Coefficient of variation", y="")

grid.arrange(revenue_histogram, market_share_histogram, avg_inefficiency_histogram, cv_inefficiency_histogram, avg_pwsby_histogram, cv_pwsby_histogram, avg_offness_histogram, cv_offness_histogram, nrow=4)

# Figure 1
rev1plot <- ggplot(AEsmall, aes(avg_inefficiency, revenue)) + geom_point() + labs(title="Time to completion \nversus Revenue", x="Days/kW", y="Revenue")
rev2plot <- ggplot(AEsmall, aes(cv_inefficiency, revenue)) + geom_point() + labs(title="Variation in time to completion \nversus Revenue", x="Coefficient of variation", y="Revenue")
share1plot <- ggplot(AEsmall, aes(avg_inefficiency, market_share)) + geom_point() + labs(title="Time to completion \nversus Market share", x="Days/kW", y="Market share")
share2plot <- ggplot(AEsmall, aes(cv_inefficiency, market_share)) + geom_point() + labs(title="Variation in time to completion \nversus Market share", x="Coefficient of variation", y="Market share")
grid.arrange(avg_inefficiency_histogram,cv_inefficiency_histogram, rev1plot, rev2plot, share1plot, share2plot,nrow=3, top="Figure 1: Time to completion variables")

# Figure 2
rev3plot <- ggplot(AEsmall, aes(avg_pwsby, revenue)) + geom_point() + labs(title="Per watt cost \nversus Revenue", x="Days/kW", y="Revenue")
rev4plot <- ggplot(AEsmall, aes(cv_pwsby, revenue)) + geom_point() + labs(title="Variation in Per watt cost \nversus Revenue", x="Coefficient of variation", y="Revenue")
share3plot <- ggplot(AEsmall, aes(avg_pwsby, market_share)) + geom_point() + labs(title="Per watt cost \nversus Market share", x="Days/kW", y="Market share")
share4plot <- ggplot(AEsmall, aes(cv_pwsby, market_share)) + geom_point() + labs(title="Variation in Per watt cost \nversus Market share", x="Coefficient of variation", y="Market share")
grid.arrange(avg_pwsby_histogram,cv_pwsby_histogram, rev3plot, rev4plot, share3plot, share4plot,nrow=3, top="Figure 2: Per watt cost variables")

# Figure 3 
rev5plot <- ggplot(AEsmall, aes(avg_offness, revenue)) + geom_point() + labs(title="Cost estimate error \nversus Revenue", x="Days/kW", y="Revenue")
rev6plot <- ggplot(AEsmall, aes(cv_offness, revenue)) + geom_point() + labs(title="Variation in Cost estimate error \nversus Revenue", x="Coefficient of variation", y="Revenue")
share5plot <- ggplot(AEsmall, aes(avg_offness, market_share)) + geom_point() + labs(title="Cost estimate error \nversus Market share", x="Days/kW", y="Market share")
share6plot <- ggplot(AEsmall, aes(cv_offness, market_share)) + geom_point() + labs(title="Variation in Cost estimate error \nversus Market share", x="Coefficient of variation", y="Market share")
grid.arrange(avg_offness_histogram,cv_offness_histogram, rev5plot, rev6plot, share5plot, share6plot,nrow=3, top="Figure 3: Cost estimate error variables")



#### Revenue models ####
# Naive model
summary(lm(revenue ~ avg_inefficiency + cv_inefficiency + avg_pwsby + cv_pwsby + avg_offness + cv_offness, data=AEsmall))
# Yay stars! 

# Naive panel model
RevModel1 <- plm(revenue ~ 
      lag(avg_inefficiency) + 
      lag(avg_pwsby) + 
      lag(avg_offness) + 
      lag(cv_inefficiency) + 
      lag(cv_pwsby) + 
      lag(cv_offness), 
      data = AEsmall, index = c("Alias","year"), effect="twoways")
summary(RevModel1)
summary(fixef(RevModel1, effect = "time"))

# One star

# With polynomials
RevModel2 <- plm(revenue ~ 
              lag(poly(avg_inefficiency,2)) + 
              lag(poly(avg_pwsby,2)) + 
              lag(poly(avg_offness,2)) + 
              lag(cv_inefficiency) + 
              lag(cv_pwsby) + 
              lag(cv_offness), 
            data = AEsmall, index = c("Alias","year"), effect="twoways")

RevModel3 <- plm(revenue ~ 
                 lag(cv_inefficiency), 
               data = AEsmall, index = c("Alias","year"), effect="twoways")
summary(RevModel3)

stargazer(RevModel1, RevModel2, RevModel3, type="text",
          covariate.labels=c(
            "Mean time to completion",
            "Mean per watt cost",
            "Mean cost estimate error",
            "Variation in time to completion",
            "Variation in per watt cost",
            "Mean time to completion", 
            "Variation in cost estimate error",
            "Mean time to completion (squared)", 
            "Mean per watt cost",
            "Mean per watt cost (squared)",  
            "Mean estimate error",  
            "Mean cost estimate error(squared)"
          ), 
          order=c(1,2,3,10,11,12),
          out="revmodels.htm"
)

#### Market share models #### 
# All in but with market share as a variable 
ShareModel1 <- plm(market_share ~ 
              lag(avg_inefficiency) + 
              lag(avg_pwsby) + 
              lag(avg_offness) + 
              lag(cv_inefficiency) + 
              lag(cv_pwsby) + 
              lag(cv_offness), 
            data = AEsmall, index = c("Alias","year"), effect="twoways")
summary(ShareModel1) 
summary(fixef(ShareModel1, effect = "time"))
summary(fixef(ShareModel1))

ShareModel2 <- plm(market_share ~ 
                     lag(poly(avg_inefficiency,2)) + 
                     lag(poly(avg_pwsby,2)) + 
                     lag(poly(avg_offness,2)) + 
                     lag(cv_inefficiency) + 
                     lag(cv_pwsby) + 
                     lag(cv_offness), 
                   data = AEsmall, index = c("Alias","year"), effect="twoways")
summary(ShareModel2)

stargazer(ShareModel1, ShareModel2, type="text",
          covariate.labels=c(
            "Mean time to completion",
            "Mean per watt cost",
            "Mean cost estimate error",
            "Variation in time to completion",
            "Variation in per watt cost",
            "Mean time to completion",
            "Variation in cost estimate error",
            "Mean time to completion (squared)",
            "Mean per watt cost",
            "Mean per watt cost (squared)",
            "Mean estimate error",
            "Mean cost estimate error(squared)"
          ),
          order=c(1,2,3,10,11,12),
          out="sharemodels.htm"
)

#### Other stuff ####
# Testing 
summary(lm(revenue ~ number_of_installs, data=AEsmall))
summary(lm(market_share ~ number_of_installs, data=AEsmall))
revmodel <- plm(revenue ~ 
  lag(market_share),
  data = AEsmall, 
  index = c("Alias","year"), 
  effect="twoways")
summary(revmodel)

sharemodel <- plm(market_share ~ 
                  lag(revenue),
                data = AEsmall, 
                index = c("Alias","year"), 
                effect="twoways")
summary(sharemodel)

summary(plm(revenue ~ lag(revenue), data=AEsmall))
summary(plm(market_share ~ lag(market_share), data=AEsmall))
summary(plm(revenue ~ lag(avg_inefficiency), data=AEsmall))
summary(plm(revenue ~ lag(market_share), data=AEsmall)) # Stars! 
summary(plm(market_share ~ lag(revenue), data=AEsmall)) # None! 

# Underlying variables
kWAC_histogram <- ggplot(AEbig, aes(`kW AC`)) + geom_histogram()
Final_inspection_histogram <- ggplot(AEbig, aes(`Final Inspection Date`)) + geom_histogram()
App_received_histogram <- ggplot(AEbig, aes(`App Received Date`)) + geom_histogram()
Total_payment_histogram <- ggplot(AEbig, aes(`Total Payment`)) + geom_histogram()
LOI_amount_histogram <- ggplot(AEbig, aes(`LOI Amount`)) + geom_histogram()
Days_to_install_histogram <- ggplot(AEbig, aes(Days_to_install)) + geom_histogram()
offness_histogram <- ggplot(AEbig, aes(offness)) + geom_histogram()
grid.arrange(kWAC_histogram, Final_inspection_histogram,App_received_histogram,Total_payment_histogram,LOI_amount_histogram,Days_to_install_histogram,offness_histogram,nrow=4)

# Scatterplots, maybe? 
# Revenue v. market_share
ggplot(AEsmall, aes(revenue, market_share, color=year)) + geom_point() + scale_colour_gradient(low = "red",high = "dark green")
cor(AEsmall$revenue, AEsmall$market_share, method="pearson")

summary(plm(revenue~ 
    lag(market_share) + 
    lag(cv_inefficiency),
    data = AEsmall, 
    index = c("Alias","year"), 
    effect="twoways"))

#### Next ####
# What to do with extreme values? Can't remove whole observations...
# How to show scatterplots with a lag (create new variables?) 
