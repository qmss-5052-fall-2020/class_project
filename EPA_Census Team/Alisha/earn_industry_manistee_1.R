library(plm)
library(plyr)
library(ggplot2)
library(tidyverse)
library(bea.R)
#install.packages(httr)
library(httr)

beakey = '386344DA-444A-4E59-A4FC-4DA15935C1A3'

paramvals <- beaParamVals(beakey, 'Regional', 'LineCode' )
paramvals <- as.data.frame(paramvals)


line1 = c( '400', '500', '600', '700', '1000', '1100', '1200', '1700','1800')


beaearnindtotal<- data.frame()


for (i in line1) {
  beaspecs2 <- list(
    'UserID' = beakey,
    'Method' = 'GetData', 
    'DatasetName' = 'Regional',
    'TableName' = 'CAINC5N',
    'LineCode' = i,
    'GeoFips' = '26101',
    'Year' = '2013,2014,2015,2016,2017,2018',
    'ResultFormat' = 'json'
  )
  beaearningsind <- beaGet(beaspecs2, asWide = FALSE)
  beaearnindtotal <- rbind(beaearnindtotal, beaearningsind, fill=TRUE)
}

by_year<- beaearnindtotal %>% group_by(TimePeriod)

earn_plot<- ggplot(beaearnindtotal, aes(x = TimePeriod , y= DataValue, fill=factor(Code))) + ggtitle("Earnings Per Industry for Manistee County, Michigan") +
  geom_bar(stat= "identity", position="dodge") +
  scale_fill_discrete(name="Industry",
                      labels=c("CAINC5N-400" = "Construction", 
                               "CAINC5N-500" = "Manufacturing", "CAINC5N-600" = "Wholesale trade", "CAINC5N-700" = "Retail trade",
                               "CAINC5N-1000" = "Finance and insurance",
                               "CAINC5N-1100" = "Real estate and rental and leasing", "CAINC5N-1200" = "Professional, scientific, and technical services",
                              "CAINC5N-1700" = "Arts, entertainment, and recreation", "CAINC5N-1800" = "Accommodation and food services")) +   
  labs(
    x = "Year",
    y = "Earnings per Industry"
  )

earn_plot

#ggplotly(earn_plot) <- commented this out because I'm not sure why but when I run this all of my legend labels dissapear

