install.packages("plm")
install.packages("car")
install.packages("gplots")
install.packages("tseries")
install.packages("lmtest")
install.packages("dummies")
finaldata <- read_excel("~/Desktop/final .xlsx")
as.data.frame(finaldata)
library(dplyr)
final <- finaldata %>% arrange(CNTY_NUM)
final1 <- final %>% mutate(un = UNEMP/100, logpop = ((log(POP)/10)), loginc = (log(PERCAPIN)/10))
as.data.frame(final1)
as.data.frame.numeric(final1)
library(plm)
data_panel <- pdata.frame(final1, index=c("CNTY_NUM","Year"))
attach(data_panel)
fixed <- plm(GINI ~ un + logpop - loginc + EDU + POVERTY, data = data_panel, effects = "twoway", model = "within")
summary(fixed)
fixed1 <- plm(GINI ~ un + loginc + EDU + POVERTY, data = data_panel, effects = "twoway", model = "within")
summary(fixed1)
data_panel1 <- transform(data_panel, Dummy1990 = as.factor(D1990), Dummy2000 = as.factor(D2000), Dummy2010 = as.factor(D2010))
class(data_panel1$Dummy2010)
attach(data_panel1)

panel_final <- pdata.frame(data_panel1, index=c("CNTY_NUM","Year"))
attach(panel_final)
fixed3 <- plm(GINI ~ un + loginc + EDU + POVERTY + Dummy1990 + Dummy2010 + Dummy2000, data = panel_final, effects = "twoway", model = "within")
summary(fixed3)

####### model testing

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
