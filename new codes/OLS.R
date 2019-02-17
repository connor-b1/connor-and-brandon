library(readr)
library("plm") 
library("car")  
library("gplots") 
library("dplyr")
library("tseries")
library("lmtest") 
install.packages("stepwise")
library(stepwise)
install.packages("olsrr")
library("olsrr")
gini <- read_xlsx("final data set-vert2.0.xlsx")
View(gini)


#OLS MODEL

#VERTICAL with dummies

library(readr)
data <- read_csv("final data set-vert2.0.csv", 
                                   col_types = cols(COUNTY = col_skip(), 
                                                    EDU = col_number(), GINI = col_number(), 
                                                    INC.Q1 = col_number(), INC.Q2 = col_number(), 
                                                    INC.Q3 = col_number(), INC.Q4 = col_number(), 
                                                    INC.Q5 = col_number(), PERCAPIN = col_number(), 
                                                    POP = col_number(), POVERTY = col_number(), 
                                                    STATE = col_skip(), UNEMP = col_number(), 
                                                    Year = col_date(format = "%Y")))

#data manipulation

datafinal <- data %>% arrange(CNTY_NUM)
datafinal <- data %>% mutate(
  UNEMP/100, 
  EDU/100,
  POVERTY/100,
  logincome = log(PERCAPIN),
  logpop = log(POP))

data_panel1 <- transform(datafinal, Dummy1990 = as.factor(D1990), Dummy2000 = as.factor(D2000), Dummy2010 = as.factor(D2010))
class(data_panel1$Dummy2010)
yes <- as.data.frame(data_panel1)

#regression models

#without magnitude

attach(yes)
OLS1 <- lm(GINI~logpop+UNEMP.100+EDU.100+POVERTY.100+logincome+Dummy1990+Dummy2000+Dummy2010)
OLS2 <- lm(GINI~logpop+UNEMP.100+EDU.100+POVERTY.100+logincome)
OLS3 <- lm(GINI~logpop+UNEMP.100+POVERTY.100+logincome)

summary(OLS1)
summary(OLS2)
summary(OLS3)
??ols_step_all_possible
ols_step_all_possible(OLS1)
#control for magnitude

final1 <- yes %>% mutate(un = UNEMP/100, logpop = ((log(POP)/10)), loginc = (log(PERCAPIN)/10))
yes2 <- as.data.frame(final1)

attach(yes2)
OLS1 <- lm(GINI~logpop+EDU.100+POVERTY.100+loginc+Dummy1990+Dummy2000)
OLS2 <- lm(GINI~logpop+UNEMP.100+POVERTY.100+loginc+EDU.100)
OLS3 <- lm(GINI~logpop+UNEMP.100+POVERTY.100+logincome)

summary(OLS1)
summary(OLS2)
summary(OLS3)

#HORIZONTAL without dummies

library(readr)
final_data_set_horiz1_0 <- read_csv("final data set-horiz1.0.csv", 
                                    col_types = cols(`1990.1quin` = col_number(), 
                                                     `1990.2quin` = col_number(), `1990.3quin` = col_number(), 
                                                     `1990.4quin` = col_number(), `1990.5quin` = col_number(), 
                                                     `1990.educ` = col_number(), `1990.gini` = col_number(), 
                                                     `1990.percapinc` = col_number(), 
                                                     `1990.pop` = col_number(), `1990.povert` = col_number(), 
                                                     `1990.unemp` = col_number(), `2000.1quin` = col_number(), 
                                                     `2000.2quin` = col_number(), `2000.3quin` = col_number(), 
                                                     `2000.4quin` = col_number(), `2000.educ` = col_number(), 
                                                     `2000.gini` = col_number(), `2000.percapinc` = col_number(), 
                                                     `2000.pop` = col_number(), `2000.povert` = col_number(), 
                                                     `2000.unemp` = col_number(), `2010.1quin` = col_number(), 
                                                     `2010.2quin` = col_number(), `2010.3quin` = col_number(), 
                                                     `2010.4quin` = col_number(), `2010.5quin` = col_number(), 
                                                     `2010.educ` = col_number(), `2010.gini` = col_number(), 
                                                     `2010.percapinc` = col_number(), 
                                                     `2010.pop` = col_number(), `2010.povert` = col_number(), 
                                                     `2010.unemp` = col_number(), CNTY_NUM = col_integer(), 
                                                     SOUCE1 = col_skip(), SOURCE2 = col_skip(), 
                                                     SOURCE3 = col_skip(), `Sources:` = col_skip(), 
                                                     X37 = col_skip(), X38 = col_skip(), 
                                                     X39 = col_skip(), X40 = col_skip(), 
                                                     X41 = col_skip(), X42 = col_skip()))
View(final_data_set_horiz1_0)

datos<- final_data_set_horiz1_0
##data manipulation

datosfinales <- datos %>% mutate(
  `1990.unemp`/100,
  `2000.unemp`/100,
  `2010.unemp`/100,
  `1990.educ`/100,
  `2000.educ`/100,
  `2010.educ`/100,
  `1990.povert`/100,
  `2000.povert`/100,
  `2010.povert`/100,
  logincome90 = log(`1990.percapinc`),
  logincome00 = log(`2000.percapinc`),
  logincome10 = log(`2010.percapinc`),
  logpop90 = log(`1990.pop`),
  logpop00 = log(`2000.pop`),
  logpop10 = log(`2010.pop`))

names(datosfinales)[37]<-"unemp90"
names(datosfinales)[38]<-"unemp00"
names(datosfinales)[39]<-"unemp10"
names(datosfinales)[40]<-"educ90"
names(datosfinales)[41]<-"educ00"
names(datosfinales)[42]<-"educ10"
names(datosfinales)[43]<-"povt90"
names(datosfinales)[44]<-"povt00"
names(datosfinales)[45]<-"povt10"

datosfinales

si <- as.data.frame(datosfinales)



#regression models

#no control for magnitude

attach(datosfinales)
#1990

MCO90 <- lm(`1990.gini` ~ povt90 + educ90 +unemp90+logincome90+logpop90)
summary(MCO90)

#2000

MCO00 <- lm(`2000.gini` ~ povt00 + educ00 +unemp00+logincome00+logpop00)
summary(MCO00)


#2010
MCO10 <- lm(`2010.gini` ~ povt10 + educ10 +unemp10+logincome10+logpop10)
summary(MCO10)

##control for magnitude

datosfinales90 <- datosfinales %>% mutate(logpop901 = (logpop90/10), loginc901 = (logincome90/10))
datosfinales00 <- datosfinales %>% mutate(logpop001 = (logpop00/10), loginc001 = (logincome00/10))
datosfinales10 <- datosfinales %>% mutate(logpop101 = (logpop10/10), loginc101 = (logincome10/10))

attach(datosfinales90)

#1990
#attach(datosfinales90)
#MCO901 <- lm(`1990.gini` ~ povt90 + educ90 +unemp901+loginc901+logpop901)
#summary(MCO901)

#2000
#attach(datosfinales00)
#MCO001 <- lm(`2000.gini` ~ povt00 + educ00 +unemp001+loginc001+logpop001)
#summary(MCO001)


#2010
#attach(datosfinales10)
#MCO101 <- lm(`2010.gini` ~ povt10 + educ10 +unemp101+loginc101+logpop101)
#summary(MCO101)

##### testing 
# looks like some NA values at the bottom of the csv were picked up

