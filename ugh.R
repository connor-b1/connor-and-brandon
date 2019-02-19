install.packages("GGally")
install.packages("clubSandwich")
library("clubSandwich")
data <- final_data_set_vert2_0 %>% arrange(CNTY_NUM)                   

date1 = seq(1:900)
plot(date1, GINI)

data2 <- data %>% arrange(CNTY_NUM)

PERCAPIN
View(final1$D2000)
attach(data)
class(data$D1990)
logpop = log(POP)
newdata <- factor(STATE)
loginc = log(PERCAPIN)
final1 <- data2 %>% mutate(logpop = (log(POP)), loginc = (log(PERCAPIN)), unemp = (UNEMP/100))
final2 <- final1[ ,-20]

#first regressions without diff()
attach(final2)
ols1 <- lm(GINI~logpop+EDU+POVERTY+loginc+unemp+factor(YEAR)+factor(STATE))
ols3 <- lm(GINI~logpop+EDU+POVERTY+factor(YEAR)+factor(STATE))
summary(ols1)
summary(ols1, cluster=c("YEAR"))
par(mfrow=c(3,3)) 
plot(ols1)

#testing Farrar ??? Glauber test (F ??? test) poverty edu and unemp ?????? 
#non differenced: logpop loginc and factors 

AIC(ols1)
BIC(ols1)
vif(ols1)
plot(ols1)

#differencing

date = seq(1:900)
par(mar = rep(2,4))

plot(date, final2$unemp, type='l',col=4)
plot(date, final2$GINI, type='l',col=4)
plot(date, final2$, type='l',col=4)

attach(final2)
dlogpvty <- diff(POVERTY)
dedu <- diff(EDU)
dunemp <- diff(unemp)

date1 = seq(1:899)
plot(date, POVERTY, type='l',col=4)

dfctyr <- factor(YEAR)
dfctst <- factor(STATE)
dfctyr1 <- dfctyr[1:899]
dfctst1 <- dfctst[1:899]
logpop899 <- logpop[1:899]
loginc899 <- loginc[1:899]

ols4 <- lm(dGINI~logpop899+dedu+dlogpvty+dunemp+loginc899+dfctyr1+dfctst1)
summary(ols4)
AIC(ols4)
BIC(ols4)
vif(ols4)
plot(ols4)






is.na.data.frame(final2)
final2 <- final1[ ,-20]
plot(final1,loginc,type='l',col=4)
anova(ols1, ols3)
anova(ols3, ols1)
ols2 <- lm(GINI~dlogpop+dedu+loginc+POVERTY) 
summary(ols2)
AIC(ols2)
BIC(ols2)
vif(ols2)
