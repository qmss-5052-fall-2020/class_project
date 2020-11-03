
#Data from Bureau of Economic Analysis - https://www.bea.gov/data/gdp/gdp-county-metro-and-other-areas - table CAGDP1

library(plm)
library(plyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(bea.R)


beakey = '386344DA-444A-4E59-A4FC-4DA15935C1A3'

beaspecs <- list(
  'UserID' = beakey,
  'Method' = 'GetData',
  'DatasetName' = 'Regional',
  'TableName' = 'CAGDP1',
  'LineCode' = '1',
  'GeoFips' = '26101',
  'Year' = 'ALL',
  'ResultFormat' = 'json'
)

beagdp <- beaGet(beaspecs, asWide = FALSE)


Manistee_GDP_plot <- ggplot(data=beagdp, aes(x = TimePeriod, y = DataValue, group= 1)) +
  ggtitle("GDP of Manistee County, Michigan") +
  geom_line() +
  geom_point() +
  labs(
    x = "Year",
    y = "Thousands of Chained 2012 Dollars"
  )

Manistee_GDP_plot

ggplotly(Manistee_GDP_plot)
