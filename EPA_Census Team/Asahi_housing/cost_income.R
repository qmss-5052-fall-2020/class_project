library(data.table)
library(dplyr)
library(extrafont)
library(ggplot2)
library(ggpubr)
library(plotly)
library(readxl)
library(sf)
library(tidycensus)

loadfonts()

api_key <- Sys.getenv("CENSUS_API_KEY")
census_api_key(api_key, install = TRUE, overwrite = TRUE)
readRenviron("~/.Renviron")

vars_acs5_2018 <- load_variables(year = 2018, dataset = "acs5")

fair_rent <- read_xlsx("Asahi_housing/data_files/FY2018_50_County_rev.xlsx")
MI_fair_rent <- fair_rent[which(fair_rent$state_alpha == "MI"),]
MI_fair_rent$GEOID <- substr(MI_fair_rent$fips2010, 0, 5)
MI_fair_rent_trim <- MI_fair_rent[ , -which(names(MI_fair_rent) %in% c("state","cbsasub18","areaname18","county","cousub","cntyname","name","pop2010","hu2010","state_alpha"
))]

MI_income_county <- get_acs(geography = "county",
                            state = "MI",
                            variables = "B19001_001",
                            year = 2018,
                            output = "wide",
                            geometry = FALSE)

MI_county_geo <- get_acs(geography = "county",
                         state = "MI",
                         variables = "B19001_001",
                         year = 2017,
                         output = "wide",
                         geometry = TRUE)[,c(1,5)]

MI_rent_income <- merge(MI_fair_rent_trim,MI_income_county,by="GEOID")
MI_rent_income$income_rent_ratio <- MI_rent_income$B19001_001E/(12*MI_rent_income$rent50_2)

MI_rent_income_geo <- st_as_sf(merge(MI_rent_income,MI_county_geo,by="GEOID"))

income_rent_plot <- ggplot(data = MI_rent_income_geo) + 
  ggtitle("Income/Rent Ratio in Michigan (2018 ACS5)") +
  labs(fill = "Income/Rent Ratio") +
  geom_sf(color = "black", aes(fill= income_rent_ratio, text = NAME)) +
  theme(
    rect = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = NA),
    plot.background = element_rect(fill = NA, color = NA),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  )

plotly_income_rent <- ggplotly(p = income_rent_plot, tooltip = "text", visible = F) %>%
  config(displaylogo = FALSE, displayModeBar = FALSE)
plotly_income_rent
