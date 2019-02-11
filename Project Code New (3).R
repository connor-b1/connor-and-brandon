#Libraries 

library(readxl)    # Reading excel file 
library(tidyverse) # Modern data science library 
library(plm)       # Panel data analysis library
library(car)       # Companion to applied regression 
library(gplots)    # Various programing tools for plotting data
library(tseries)   # For timeseries analysis
library(lmtest)    # For hetoroskedasticity analysis


#Reading in the data 
remit_data <- read_excel("C:/Users/garre/Google Drive/School/Grad/Fall 2018/ECON 8010 - Econometrics/Econometrics Project/full_remit_data_thinned.xlsx")

#Adding time trend
remit_data$trend <- remit_data$year - min(remit_data$year)

#Set data as Panel Data
remit_panel <- pdata.frame(remit_data, index = c("country","year"))
attach(remit_panel)

mean(con_deaths_prct)
sd(con_deaths_prct)



#Exploratory 
coplot( remit ~ year | country, type = "b", data = remit_panel )
scatterplot(remit ~ year|country, data = remit_panel)
plotmeans(remit ~ country, data = remit_panel)
plotmeans(remit ~ year, data = remit_panel)


########################## 
### Current Variables ####
##########################

#Fixed model 
fixed <- plm(remit ~ remit_lag + emigrant_prct + trend + nat_cost_prct + nat_aff_prct + con_deaths_prct +
               nat_cost_prct*emigrant_prct + nat_aff_prct*emigrant_prct + con_deaths_prct*emigrant_prct, 
             data = remit_panel, effects = "twoway", model = "within")
summary(fixed)


### Testing for Pooled Effects

#Fixed model for Pool Test  
fixed_vcm <- pvcm(remit ~ remit_lag + emigrant_prct + trend + nat_cost_prct + nat_aff_prct + con_deaths_prct +
               nat_cost_prct*emigrant_prct + nat_aff_prct*emigrant_prct + con_deaths_prct*emigrant_prct, 
             data = remit_panel, model = "within")


#Pooled Model 
pooled <- plm(remit ~ remit_lag + emigrant_prct + trend + nat_cost_prct + nat_aff_prct + con_deaths_prct +
                nat_cost_prct*emigrant_prct + nat_aff_prct*emigrant_prct + con_deaths_prct*emigrant_prct, 
              data = remit_panel, model = "pooling")

summary(pooled)

# Pool Test: Results in p < .05, 
 
pooltest(pooled, fixed_vcm) #Null hypothesis is that individual countries have same slop coefficients

pooltest(remit ~ remit_lag + emigrant_prct + trend + nat_cost_prct + nat_aff_prct + con_deaths_prct +
           nat_cost_prct*emigrant_prct + nat_aff_prct*emigrant_prct + con_deaths_prct*emigrant_prct, 
         data = remit_panel, model = "within") #Null hypothesis is that individual countries have same slop coefficients

pFtest(fixed, pooled) #Null hypothesis is that there are no fixed effects



### Test for two way fixed effect

#Langrange Multiplier Test : Significant effects 
plmtest(pooled, effect = "twoways", type = "ghm")


#F Test 
pFtest(fixed, pooled)


###Test if should use fixed or random effects model 

#Random effects model 
random <- plm(remit ~ remit_lag + emigrant_prct + trend + nat_cost_prct + nat_aff_prct + con_deaths_prct +
                nat_cost_prct*emigrant_prct + nat_aff_prct*emigrant_prct + con_deaths_prct*emigrant_prct, 
              data = remit_panel, model = "random")
summary(random)

# Test between two
phtest(fixed, random) #P value is significant, so we use the fixed effects/within model 



##Serial correlation testing 

#Short Panel Serial Correlation
pwartest(remit ~ remit_lag + emigrant_prct + trend + nat_cost_prct + nat_aff_prct + con_deaths_prct +
           nat_cost_prct*emigrant_prct + nat_aff_prct*emigrant_prct + con_deaths_prct*emigrant_prct,
         data = remit_panel)

#Long panel serial correlation 
pbgtest(fixed) #No serial correlation, p not less than .05, but close



##Cross sectional dependence testing
pcdtest(remit ~ remit_lag + emigrant_prct + trend + nat_cost_prct + nat_aff_prct + con_deaths_prct +
          nat_cost_prct*emigrant_prct + nat_aff_prct*emigrant_prct + con_deaths_prct*emigrant_prct, 
        data = remit_panel, test = c("cd"), model = "within", effect = "twoways") #There is cross sectional dependence, p value less than .05


##Unit roots/stationarity testing 
adf.test(remit_panel$remit, k=9) #Series is stationary, since p value is less than .05


#Heteroskedasticity testing 
bptest(remit ~ remit_lag + emigrant_prct + nat_cost_prct + nat_aff_prct + con_deaths_prct +
         nat_cost_prct*emigrant_prct + nat_aff_prct*emigrant_prct + con_deaths_prct*emigrant_prct,
       studentize = F) #There is heteroskedasticity, p values less than .05


#Arellano controls for heteroskedaticity and serial correlation 
summary(fixed, vcov = function(x) vcovHC(x, method = "arellano"))
summary(fixed)

mean(fixed$residuals^2)

fixed_res <- resid(fixed)

plot(remit, fixed_res, 
     ylab = "Residuals", xlba = "Remit", 
     main = "Remittances share of GDP")
abline(0,0) 


########################## 
### Lagged Variables ####
##########################

##Test if should use fixed or random effects model 
#Fixed model 
fixed_lag <- plm(remit ~ remit_lag + emigrant_prct + trend + nat_cost_prct_lag + nat_aff_prct_lag + con_deaths_prct_lag +
                   nat_cost_prct_lag*emigrant_prct + nat_aff_prct_lag*emigrant_prct + con_deaths_prct_lag*emigrant_prct, 
                 data = remit_panel, model = "within")
summary(fixed_lag)
fixef(fixed_lag)

#Random effects model 
random_lag <- plm(remit ~ remit_lag + emigrant_prct + trend + nat_cost_prct_lag + nat_aff_prct_lag + con_deaths_prct_lag +
                    nat_cost_prct_lag*emigrant_prct + nat_aff_prct_lag*emigrant_prct + con_deaths_prct_lag*emigrant_prct, 
                  data = remit_panel, model = "random")
summary(random_lag)

#Test between the two 
phtest(fixed_lag, random_lag) #P value is less than .05, so we use the fixed effects/within model 


##Test time fixed effects 
plmtest(fixed_lag, c("time"), type = ("bp")) #No need to to use time fixed effects: p is not less than .05


##Cross sectional dependence testing
pcdtest(fixed_lag, test = c("lm")) #There is cross sectional dependence, p value less than .05
pcdtest(fixed_lag, test = c("cd")) #There is cross sectional dependence, p value less than .05


##Serial correlation testing 
pbgtest(fixed_lag) #There is serial correlation, p value less than .05


##Unit roots/stationarity testing 
adf.test(remit_panel$remit) #Series is stationary, since p value is less than .05


#Heteroskedasticity testing 
bptest(remit ~ remit_lag + emigrant_prct + nat_cost_prct_lag + nat_aff_prct_lag + con_deaths_prct_lag +
         nat_cost_prct_lag*emigrant_prct + nat_aff_prct_lag*emigrant_prct + con_deaths_prct_lag*emigrant_prct,
       studentize = F) #There is heteroskedasticity, p value less than .05


#Arellano controls for heteroskedaticity and serial correlation 
summary(fixed_lag, vcov = function(x) vcovHC(x, method = "arellano"))

mean(fixed_lag$residuals^2)

fixed_lag_res <- resid(fixed_lag)

plot(remit, fixed_lag_res, 
     ylab = "Residuals", xlab = "Remit", 
     main = "Remittances share of GDP")
abline(0,0) 



##################################################### 
### REGION: South/Central America and Carribbean ####
#####################################################

##Read in the data

region_data <- read_excel("C:/Users/garre/Google Drive/School/Grad/Fall 2018/ECON 8010 - Econometrics/Econometrics Project/region.xlsx")

#Add time trend
region_data$trend <- region_data$year - min(region_data$year)

#Set data as Panel Data

region_panel <- pdata.frame(region_data, index = c("country","year"))
attach(region_panel)


###############
# Current

#Fixed model 
r_fixed <- plm(remit ~ remit_lag + trend + emigrant_prct + nat_cost_prct + nat_aff_prct + con_deaths_prct +
                 nat_cost_prct*emigrant_prct + nat_aff_prct*emigrant_prct + con_deaths_prct*emigrant_prct, 
               data = region_panel, model = "within")
summary(r_fixed)
fixef(r_fixed)

mean(r_fixed$residuals^2)

#Arellano controls for heteroskedaticity and serial correlation 
summary(r_fixed, vcov = function(x) vcovHC(x, method = "arellano"))



###############
# Lagged

#Fixed model 
r_fixed_lag <- plm(remit ~ remit_lag + emigrant_prct +  trend + nat_cost_prct_lag + nat_aff_prct_lag + con_deaths_prct_lag +
                     nat_cost_prct_lag*emigrant_prct + nat_aff_prct_lag*emigrant_prct + con_deaths_prct_lag*emigrant_prct, 
                   data = region_panel, model = "within")
summary(r_fixed_lag)
fixef(r_fixed_lag)

mean(r_fixed_lag$residuals^2)

#Arellano controls for heteroskedaticity and serial correlation 
summary(r_fixed_lag, vcov = function(x) vcovHC(x, method = "arellano"))

