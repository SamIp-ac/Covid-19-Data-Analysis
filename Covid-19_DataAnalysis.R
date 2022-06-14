#Dataset: https://www.kaggle.com
#Covid-19 (22-1-2020 --- 27-7-2020)
library(datasets, drc)
if (!require("pacman")) install.packages("pacman") #pacman to load add-on packages as desired
pacman::p_load(pacman, tidyverse, psych, stats, lars, caret, MASS, standardize, drc) 

USA_country_wise <- read.csv('~/Desktop/Covid-19/usa_county_wise.csv', header = TRUE)
worldometer_data <- read.csv('~/Desktop/Covid-19/worldometer_data.csv', header = TRUE)
full_grouped <- read.csv('~/Desktop/Covid-19/full_grouped.csv', header = TRUE)
covid_19_clean_complete <- read.csv('~/Desktop/Covid-19/covid_19_clean_complete.csv', header = TRUE)
day_wise <- read.csv('~/Desktop/Covid-19/day_wise.csv', header = TRUE)

head(USA_country_wise)
head(worldometer_data)
head(full_grouped)
head(covid_19_clean_complete)
head(day_wise)
######
summary(day_wise)
typeof(day_wise)
head(day_wise)

#Overview
summary(day_wise)
day_wise_Confiremed_plot <- plot(day_wise$Confirmed/10**6, main = 'Day Wise Confirmed',
                                 type = 'l', ylab = 'Cases (in million)',xlab = 'Number of days (From 22-1-2020)')
abline(v = 60)
abline(v = 70)
#increasing around 26-3-2020 (1)
No..of.countries <- plot(day_wise$No..of.countries, main = 'Number of countries affected', type = 'l',
                         xlab = 'Number of days  (From 22-1-2020)', ylab = 'Number of Countries')
abline(v = 33)

#increasing after 24-2-2020 (2)
#Diff. countries' comparasion
full_grouped_USA <- full_grouped[full_grouped$Country.Region == 'US',]
head(full_grouped_USA)
full_grouped_China <- full_grouped[full_grouped$Country.Region == 'China',]
head(full_grouped_China)
full_grouped_India <- full_grouped[full_grouped$Country.Region == 'India',]
head(full_grouped_India)
full_grouped_Russia <- full_grouped[full_grouped$Country.Region == 'Russia',]
head(full_grouped_Russia)
full_grouped_Brazil <- full_grouped[full_grouped$Country.Region == 'Brazil',]
head(full_grouped_Brazil)
full_grouped_UnitedKingdom <- full_grouped[full_grouped$Country.Region == 'United Kingdom',]
head(full_grouped_UnitedKingdom)
full_grouped_Italy <- full_grouped[full_grouped$Country.Region == 'Italy',]
head(full_grouped_Italy)

#plot
days <- c(1:188)
days
full_grouped_USA_plot <- plot(full_grouped_USA$Confirmed/10**6, col = 'blue',
                              type = 'l', main = 'Day Wise Confirmed',
                              ylab = 'Cases(in million)', xlab = 'Number of days (Starting from 2020-1-22)')

lines(days,full_grouped_China$Confirmed/10**6, col = 'red') 
lines(days,full_grouped_India$Confirmed/10**6, col = 'brown')
lines(days,full_grouped_Russia$Confirmed/10**6, col = 'green')
lines(days,full_grouped_Brazil$Confirmed/10**6, col = 'orange')
lines(days,full_grouped_UnitedKingdom$Confirmed/10**6, col = 'violet')
lines(days,full_grouped_Italy$Confirmed/10**6, col = 'Pink')
legend(1, 4, legend=c("USA", "China", 'India', 'Russia', 'Brazil', 'UK', 'Italy'),
       col=c("blue", "red", 'brown', 'green', 'orange', 'violet', 'Pink'), lty=1)
abline( v = 60)
#
#Unused
ts_day_wise <- ts(day_wise)
full_grouped_China_plot <- plot(days,full_grouped_China$Confirmed, col = 'red',
                                type = 'l', main = 'Day Wise Confirmed',
                                ylab = 'Cases', xlab = 'Number of days')

#death/confirmed ratio per unit day
deaths_confirmed_ratio <- rep(0,188)
for (i in c(1:188)){
  deaths_confirmed_ratio[i] <- day_wise$Deaths[i]/day_wise$Confirmed[i]
}
plot(deaths_confirmed_ratio*100, type = 'l', 
     main = 'Covid-19 Deaths-Confirmed Ratio variation within 188-days',
     xlab = 'Number of days', ylab = 'Death (per 100 cases)', col = 'red')
lines(days, day_wise$Deaths...100.Cases)
# Got the same graph !!
x <- max(deaths_confirmed_ratio)
which(x == deaths_confirmed_ratio)
deaths_confirmed_ratio[99]
abline(h = x*100)
points(99, x*100)
day_wise$Date[99] # = 2020-04-29
#Decrease after the 99-th days
# Around 2020-04: Many vaccines undergo clinical experiments
# To calculate the death rate
deaths_confirmed_ratio =  day_wise$Deaths[188]/day_wise$Confirmed[188]
deaths_confirmed_ratio
deaths_confirmed_rate_now  = (3.77/175)
deaths_confirmed_rate_now
#The death-confirmed rate now is 2.154%, as the vaccine widespread.
#Which country is lessen the most after vaccines are invented
#To find out which vaccine is the most efficitive


######
######PCA

#######Claim: Deaths data relate to other component

#principal comp. analysis
PCA1 <- prcomp( ~ worldometer_data$Population + worldometer_data$TotalCases + worldometer_data$TotalDeaths +
                  worldometer_data$TotalTests + worldometer_data$TotalRecovered + worldometer_data$ActiveCases
                , center = TRUE, scale = TRUE)
plot(PCA1)
summary(PCA1)
PCA1
predict(PCA1) %>% round(2)
biplot(PCA1)


worldometer_data1 <- worldometer_data[4:209,]
head(worldometer_data1)
PCA2 <- prcomp( ~ worldometer_data1$Population + worldometer_data1$TotalCases + worldometer_data1$TotalDeaths +
                  worldometer_data1$TotalTests + worldometer_data1$TotalRecovered + worldometer_data1$ActiveCases
                , center = TRUE, scale = TRUE)
plot(PCA2)
summary(PCA2)
PCA2
predict(PCA2) %>% round(2)
biplot(PCA2)

worldometer_data2 <- worldometer_data1[4:205,]
head(worldometer_data2)
PCA3 <- prcomp( ~ worldometer_data2$Population + worldometer_data2$TotalCases + worldometer_data2$TotalDeaths +
                  worldometer_data2$TotalTests + worldometer_data2$TotalRecovered 
                + worldometer_data2$ActiveCases
                , center = TRUE, scale = TRUE)
plot(PCA3)
summary(PCA3)
PCA3
predict(PCA3) %>% round(2)
biplot(PCA3)

lm0 <- lm(worldometer_data2$TotalDeaths ~ worldometer_data2$Population + worldometer_data2$TotalCases
          + worldometer_data2$TotalTests + worldometer_data2$TotalRecovered + worldometer_data2$ActiveCases)

summary(lm0)
anova(lm0)            # Coefficients w/inferential tests
coef(lm0)             # Coefficients (same as reg1)
confint(lm0)          # CI for coefficients
resid(lm0)            # Residuals case-by-case
hist(residuals(lm0))  # Histogram of residuals

lm1 <- lm(worldometer_data2$TotalDeaths ~  + worldometer_data2$ActiveCases + worldometer_data2$TotalCases
          + worldometer_data2$TotalTests + worldometer_data2$TotalRecovered)

summary(lm1)
anova(lm1)            # Coefficients w/inferential tests
coef(lm1)             # Coefficients (same as reg1)
confint(lm1)          # CI for coefficients
resid(lm1)            # Residuals case-by-case
hist(residuals(lm1),28)  # Histogram of residuals

#1
day_wise_en <- day_wise

pc_CDRA <-prcomp(~ Confirmed + Deaths + Recovered + Active, 
                 data = day_wise_en, center = TRUE, scale = TRUE)
plot(pc_CDRA)
summary(pc_CDRA)
pc_CDRA
predict(pc_CDRA) %>% round(2)
biplot(pc_CDRA)
######Confirmed + Deaths + Recovered + Active
head(day_wise_en)
pc_CDRA <-prcomp(~ Confirmed + Deaths + Recovered + Active, 
                 data = day_wise_en, center = TRUE, scale = TRUE)
plot(pc_CDRA)
summary(pc_CDRA)
pc_CDRA
predict(pc_CDRA) %>% round(2)
biplot(pc_CDRA)
#By PCA, relation between Active and Deaths are too close, use Deaths ~ Recovered +  Confirmed will be better
#TRY first: linear modeling Recovered + Active + Confirmed
lm_CDRA <- lm(Deaths ~ Recovered + Active + Confirmed, data = day_wise_en)
summary(lm_CDRA)
anova(lm_CDRA)            # Coefficients w/inferential tests
coef(lm_CDRA)             # Coefficients (same as reg1)
confint(lm_CDRA)          # CI for coefficients
resid(lm_CDRA)            # Residuals case-by-case
hist(residuals(lm_CDRA))  # Histogram of residuals
#Both two R-squared = 1
#ANOVA F-tests on an essentially perfect fit are unreliable, rejected.
#linear modeling Recovered + Active + Confirmed
lm_CDRA <- lm(Deaths ~ Recovered + Confirmed, data = day_wise_en)
summary(lm_CDRA)
anova(lm_CDRA)            # Coefficients w/inferential tests
coef(lm_CDRA)             # Coefficients (same as reg1)
confint(lm_CDRA)          # CI for coefficients
resid(lm_CDRA)            # Residuals case-by-case
hist(residuals(lm_CDRA))  # Histogram of residuals
#input data from google: 12-6-2021 India
new_recovered_confirmed_India <- data.frame(Recovered = c(27800000), Confirmed = c(29300000))
new_deaths_India <- predict(lm_CDRA, newdata = new_recovered_confirmed_India)
new_deaths_India
#It fail, Since the data no just India, and it maybe non-linear, and the change of human hygiene awareness
#2
head(day_wise_en)
pc_all <-prcomp(~ Confirmed + Deaths + Recovered + Active + Deaths...100.Cases +
                  Recovered...100.Cases + Deaths...100.Recovered + 
                  New.cases + New.deaths + New.recovered, 
                data = day_wise_en, center = TRUE, scale = TRUE)
plot(pc_all)
summary(pc_all)
pc_all
predict(pc_all) %>% round(2)
biplot(pc_all)
#linear modeling
# - active - new case + deaths...100.R + Death...100.C + New. d + Recovered + Confirmed (can be change)
lm_all <- lm(Deaths ~ Confirmed + Recovered + Deaths...100.Cases 
             + Deaths...100.Recovered + New.deaths, data = day_wise_en)
summary(lm_all)
anova(lm_all)            # Coefficients w/inferential tests
coef(lm_all)             # Coefficients (same as reg1)
confint(lm_all)          # CI for coefficients
resid(lm_all)            # Residuals case-by-case
hist(residuals(lm_all))  # Histogram of residuals
##input data from google: 12-6-2021 India
new_recovered_confirmed_India <- data.frame(Recovered = c(27800000), Confirmed = c(29300000),
                                            Deaths...100.Cases = c(), New.deaths = c(7374),
                                            Deaths...100.Recovered = c())
new_deaths_India <- predict(lm_CDRA, newdata = new_recovered_confirmed_India)
new_deaths_India

######New.cases + New.deaths +New.recovered,data
pcN.cdr <-prcomp(~ New.cases + New.deaths + New.recovered,
                 data = day_wise_en, center = TRUE, scale = TRUE)
plot(pcN.cdr)
summary(pcN.cdr)
pcN.cdr
predict(pcN.cdr) %>% round(2)
biplot(pcN.cdr)

######
rm(list = ls())
cat("/014")
######
dev.off()

#Unused
standardize(~Confirmed + Deaths + Recovered + Active + New.cases + New.deaths +
              New.recovered + Deaths...100.Cases +
              Recovered...100.Cases,
            data = day_wise)
day_wise_en$Deaths...100.Recovered <- scale(day_wise_en$Deaths...100.Recovered)