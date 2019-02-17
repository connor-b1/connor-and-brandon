library(readxl)
library(dplyr)
library(ggplot2)
install.packages(ggmap)
install.packages(rgdal)

library(choroplethr)
library(choroplethrMaps)

tmp = tempfile()
download.file(url = "http://www.stats.indiana.edu/maptools/county_zips.xls", 
              destfile = tmp)
countyZIP <- read_excel(tmp, sheet = 2, skip = 1)
names(countyZIP) <- c("FIPS", "county", "zip", "percent_zip_in_county")

read.csv(final_data_set-horiz1.0.csv)

data<- final_data_set_horiz1_0
data90 <- (data$`1990.gini`)
data90.IN <- data90$`data$`1990.gini``[c(1:92),c(2,0)]
dataGINI90<- data$`1990.gini`
dataGINI00<- data$`2000.gini`
dataGINI10<- data$`2010.gini`
county <- countyZIP$county
county <- group_by(countyZIP, county)
county <- summarize(county, value = N1)
#organize counties
county <- 

as.factor()
#set parameters 
dataIN90 <- dataGINI90[(1:92)]
dataIN00 <- dataGINI00[(1:92)]
dataIN10 <- dataGINI10[(1:92)]
dataKY90 <- dataGINI90[(93:212)]
dataKY00 <- dataGINI00[(93:212)]
dataKY10 <- dataGINI10[(93:212)]
dataOH90 <- dataGINI90[(213:300)]
dataOH00 <- dataGINI00[(213:300)]
dataOH10 <- dataGINI10[(213:300)]
IN90 <- as.character(dataIN90)
IN00 <- as.character(dataIN00)
IN10 <- as.character(dataIN10)
KY90 <- as.character(dataKY90)
KY00 <- as.character(dataKY00)
KY10 <- as.character(dataKY10)
OH90 <- as.character(dataOH90)
OH00 <- as.character(dataOH00)
OH10 <- as.character(dataOH10)

#join data
county <- left_join(IN90, , by = c("zip" = "zipcode"))

county.regionsIN <- filter(county.regions, state.name == "indiana")
county.regionsKY  <- filter(county.regions, state.name == "kentucky")
county.regionsOH  <- filter(county.regions, state.name == "ohio")

#vizualize data
#IN1990
county$county <- tolower(county$county)
data(county.regions)
county.regions <- filter(county.regions, state.name == "indiana")
county <- left_join(county, county.regions, by = c("county" = "county.name"))
county_map <- county_choropleth(county, legend = "Returns under $25,000", 
                                state_zoom = "indiana")
county_map

#IN2000

#IN2010
#KY1990
#KY2000
#KY2010
#OH1990
#OH2000
#OH2010


