
# ------------------------------------------------------------------------------------ #
# ************************************************************************************ #

# COVID-19 Intelligent Forecasting Dashboard Functions
# KPMG x QMSS Practicum 2020
# Team: Ariel Luo, Sydney Bolim Son, Andrew Thvedt, Jen Woo, Louisa Ong

# Visualizations Lead: Louisa Ong

# ------------------------------------------------------------------------------------ #
# ************************************************************************************ #



# rShiny dashboard app app.R file

# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #
# ------------------------------------------------------------------ #
# ---------------------------- SETUP ------------------------------- #
# ------------------------------------------------------------------ #
# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #

# shiny essentials
library(shiny)
library(shinythemes)
library(shinydashboard)

library(plotly) # for interactive plots 
library(tidyverse) # datacleaning
library(ggplot2) # graphing
library(RColorBrewer) # graph color palette 
library(showtext) # for fonts
library(janitor)

# spatial essentials 
library(sf) # for merging spatial datasets 
library(jsonlite) # from FIPS -> Geo (lat, long)
library(gridExtra) # for grid plot
# library(gganimate) # for interactive plot 
# library(tmap)
library(profvis)



# Fonts
showtext_auto()
font_add(family = "raleway", regular = "fonts/Raleway-Regular.ttf")
font_add(family = "instruction", regular = "fonts/Instruction.otf")
# font_add(family = "montserrat", regular = "fonts/Montserrat-Regular.ttf")
# font_add(family = "proxima-nova", regular = "fonts/ProximaNova-Reg.ttf")
# font_add(family = "proximanovalight", regular = "fonts/ProximaNova-Light.ttf")
# font_add(family = "montserratlight", regular = "fonts/Montserrat-Light.ttf")
# font_add(family = "montserratthin", regular = "fonts/Montserrat-Thin.ttf")

# Turn off scientific notation
options(scipen = 999)

# ------------------------  Importing Datasets -------------------------- # 

# US State Level Clean
covid_state <- read_csv("data/us_state_level_clean_2020-12-02.csv") # States only

# MSAs & Regional Areas 
covid <- read_csv("data/us_daily_msa_final_2020-12-02.csv") # includes MSAs and Regional Areas

# MSA fips codes 
# msa <- read_csv("data/msa.csv")
msa_index <- read_csv("data/msa_mix_index.csv") # csa and msa with population


# html viz from Andrew & Sydney ML Model Analysis 
cat_d_plot <- "viz/deaths_per_100k.html"
cat_c_plot <- "viz/cases_deaths_per_100k.html"

# spatial shape files 
county_lines <- st_read("data/county_shape/cb_2018_us_county_500k.shp")
state_lines <- st_read("data/state_shape/cb_2018_us_state_500k.shp")



# ////////////////////////// TAB 1 ///////////////////////////////// # 

# ----------- Creating State Details Table to be Displayed on Dashboard  ------------- # 

# rename for State details table (mostly totals)
# also selecting only variables to be displayed 
covid_forecast <- covid_state %>%
  filter(Date == as.Date(max(Date)))

covid_current <- covid_state %>%
    filter(Date == as.Date(min(c(Sys.Date(), max(Date)))))
  


details <- function(covid_type) {
  
  covid_type %>%
    mutate("Total Deaths per Capita" = signif(Deaths/population, 3)) %>%
    mutate("Total Cases per Capita" = signif(Confirmed/population, 3)) %>%
    
    rename("State" = Province_State) %>%
    rename("Population" = population) %>%
    
    rename("Total Deaths" = Deaths) %>%
    mutate("Total Deaths per 100k" = signif(`Total Deaths`/100000, 3)) %>% 
    rename("Total Cases" = Confirmed) %>%
    mutate("Total Cases per 100k" = signif(`Total Cases`/100000, 3)) %>% 
    mutate("Spread Category" = "Semi-Controlled") %>% # arbitrary
    mutate("Mobility Index" = round(-22)) %>% 
    mutate("Containment Index" = round(containment)) %>%
    # days since peak 
    select("State", "Population", 
    "Total Deaths", "Total Deaths per 100k", "Total Deaths per Capita", 
    "Total Cases", "Total Cases per 100k", "Total Cases per Capita", 
    "Containment Index", "Mobility Index")
  
}

details_forecast <- details(covid_forecast)

details_current <- details(covid_current)


# ------------------------ Renaming Variables for Line Plot -------------------------- # 

# Renaming so that the variable names in plotly shows up well
covid_renamed <- covid_state %>% 
  mutate("7-Day Average Deaths" = new_deaths_7) %>%
  mutate("Daily Deaths per 100k" = new_deaths_7/100000) %>%
  mutate("Daily Deaths per Capita" = covid_state$new_deaths_7/covid_state$population) %>%
  rename("Province/State" = Province_State) %>%
  mutate("7-Day Average Cases" = new_confirmed_7) %>%
  mutate("Daily Cases per 100k" = new_confirmed_7/100000) %>%
  mutate("Daily Cases per Capita" = covid_state$new_confirmed_7/covid_state$population)



# ////////////////////////// TAB 2 ///////////////////////////////// # 

# Mostly Spatial/Geo Things

# ------------------------ 1. Preparing County Data to Merge with Covid Mix dataset  -------------------------- # 

# In order to match with spatial lines
# rename variables to be able to merge well
msa_index_2 <- msa_index %>%
  rename(county = `County/County Equivalent`) %>%
  rename(csa = `CSA Title`) %>%
  rename(state = `State Name`) %>%
  rename(agg_index = mix_index) %>%
  rename(fips = FIPS_combined)


# ----------- 2. Separating Cases & Deaths Dataset Into States, States - Region Only, MSAs Only ------------- #

# joining fips info with covid dataset (has states + selected msas)
state_msa_fips_join <- covid %>%
  full_join(msa_index_2, by = "agg_index")# %>%
#
# MSA ONLY
# keeping only msas and counties, since they have fips
msa_covid_only <- state_msa_fips_join %>%
  drop_na(fips) # those without fips = are not MSAs

# STATE WITHOUT MSAs
# keeping only states, don't have counties
state_covid_only <- state_msa_fips_join %>%
  anti_join(msa_covid_only) %>% # States not in MSA only dataset
  select(-fips, -mix_index_population, -FIPS_population, -`Central/Outlying County`, -state, -county, -csa, -`Metropolitan Division Title`)


# ------------------------ 3. Separating the Three Datasets by the 3 Wave Counts -------------------------- #
#
## for States

# Wave dates
wave_2 <- as.Date("2020-06-15") # start of wave 2
wave_3 <- as.Date("2020-10-15") # start of wave 3


# FUNCTION separating COVID total data of areas into waves

waveSep <- function(covid_area) {

  # Filter out outlying states
  covid_area_2 <- covid_area %>%
    filter(! NAME %in% c("Alaska", "Hawaii", "Rhode Island", "District of Columbia"))

  # All numbers at Latest date available
  covid_area_total <- covid_area_2 %>%
    filter(Date == max(as.Date(covid_area_2$Date), na.rm = TRUE)) %>% # need to remove NA
    mutate(Date_total = Date)

  # Total numbers at the start of Wave 2
  area_w2_start <- covid_area %>%
    filter(Date == wave_2) %>%
    mutate(Date_W2 = Date)

  # Total numbers at the start of Wave 3
  area_w3_start <- covid_area_2 %>%
    filter(Date == wave_3) %>%
    mutate(Date_W3 = Date)



  # Creating Dataset with numbers for each wave
  area_waves_all <- covid_area_total %>%
    rename(Confirmed_total = Confirmed) %>% # Start - Latest
    rename(Deaths_total = Deaths) %>%

    # joining wave 2 and beyond numbers
    left_join(area_w2_start, by = "NAME") %>%  # Start - June 15

    # wave 1 only %>%
    rename(Confirmed_W1 = Confirmed) %>%  # Start - June 15
    rename(Deaths_W1 = Deaths) %>% # Start - June 15

    # wave 2 and beyond column
    mutate(Confirmed_W2_plus = Confirmed_total - Confirmed_W1) %>% # June 15 - Latest
    mutate(Deaths_W2_plus = Deaths_total - Deaths_W1) %>% # June 15 - Latest
    # joining wave 3 numbers
    left_join(area_w3_start, by = "NAME") %>% # Start - Oct 15 ie Wave 1 & Wave 2

    # only wave 2
    mutate(Deaths_W2_only = Deaths - Deaths_W1) %>% # June 15 - Oct 15
    mutate(Confirmed_W2_only = Confirmed - Confirmed_W1) %>% # June 15 - Oct 15

    # wave 3 and beyond
    mutate(Deaths_W3 = Deaths_total - Deaths) %>%  # Oct 15 - Latest
    mutate(Confirmed_W3 = Confirmed_total - Confirmed) # Oct 15 - Latest

}
#
#
# WAVES IN MSAs
msa_only_waves <- msa_covid_only %>%
  rename("NAME" = "agg_index") %>%
  waveSep() %>%
  # tidy up, rename
  rename(metro_title = "Metropolitan Division Title") %>%
  rename(central_outlying = "Central/Outlying County") %>%
  # selecting only relevant variables
  select(NAME, fips, csa, county, metro_title, population, FIPS_population, mix_index_population,
         Date_total, Confirmed_total, Deaths_total,
         Confirmed_W1, Deaths_W1,
         Date_W2, Confirmed_W2_plus, Confirmed_W2_only, Deaths_W2_plus, Deaths_W2_only,
         Date_W3, Confirmed_W3, Deaths_W3) %>%
  distinct()

# WAVES IN REGIONAL PART OF STATES (WITHOUT MSA)
state_reg_waves <- covid_state %>%
  rename("NAME" = "Province_State") %>%
  waveSep() %>%
  # tidy up, selecting only relevant variables
  select(NAME, population,
         Date_total, Confirmed_total, Deaths_total,
         Confirmed_W1, Deaths_W1,
         Date_W2, Confirmed_W2_plus, Confirmed_W2_only, Deaths_W2_plus, Deaths_W2_only,
         Date_W3, Confirmed_W3, Deaths_W3, stringency, containment, RegionCode)

# WAVES IN STATES WITHOUT MSA
state_only_waves <- state_covid_only %>%
  rename("NAME" = "agg_index") %>%
  waveSep() %>%
  # tidy up, selecting only relevant variables
  select(NAME, population,
         Date_total, Confirmed_total, Deaths_total,
         Confirmed_W1, Deaths_W1,
         Date_W2, Confirmed_W2_plus, Confirmed_W2_only, Deaths_W2_plus, Deaths_W2_only,
         Date_W3, Confirmed_W3, Deaths_W3)
#
#
#
# # ------------------------ 4. Adding Spatial Attributes for Plotting  -------------------------- #
#
#
# ADD COUNTY SPATIAL ATTRIBUTES
# add FIPS as a variable from State Fips and County Fips
county_lines <- county_lines %>%
  mutate(fips = paste(STATEFP, COUNTYFP, sep = ""))

# add lat long attributes to county dataset
county_geo <- merge(county_lines, msa_only_waves, by = "fips")
county_map <- county_geo %>% st_transform('+proj=longlat +datum=WGS84')



# ADD STATE SPATIAL ATTRIBUTES
# add lat long attributes to state dataset

# these are states spatial data with regional covid numbers
state_reg_geo <- merge(state_lines, state_reg_waves, by = "NAME")
state_reg_map <- state_reg_geo %>% st_transform('+proj=longlat +datum=WGS84')

# these are states without MSAs, hence original state values
state_geo <- merge(state_lines, state_only_waves, by = "NAME")
state_map <- state_geo %>% st_transform('+proj=longlat +datum=WGS84')
#
#
# covid_state_2 <- covid_state %>%
#   rename("NAME" = "Province_State")
#
# state_geo_all <- merge(state_lines, covid_state_2, by = "NAME")
# state_map_all <- state_geo %>% st_transform('+proj=longlat +datum=WGS84')


# g_total <- ggplot() +
#   geom_sf(data = state_reg_map,
#           aes(fill = Deaths_total/population),
#           color = NA) +  # color = NA
#   geom_sf(data = county_map, aes(fill = Deaths_total/population), color = NA) + # MSA Things
#   scale_fill_viridis_c(name = "Deaths per Capita", option = "viridis", direction = -1) +
#   labs(title = "Total Deaths\n") +
#   theme_void() +
#   theme(text = element_text(family = "raleway", size = 50))
# 
# # min max values
# max_W1 <- max(c(state_map$Deaths_W1 / state_map$population, county_map$Deaths_W1 / county_map$population))
# max_W2 <- max(c(state_map$Deaths_W2_only / state_map$population, county_map$Deaths_W2_only / county_map$population))
# max_W3 <- max(c(state_map$Deaths_W3 / state_map$population, county_map$Deaths_W3 / county_map$population))
# 
# min_W1 <- min(c(state_map$Deaths_W1 / state_map$population, county_map$Deaths_W1 / county_map$population))
# min_W2 <- min(c(state_map$Deaths_W2_only / state_map$population, county_map$Deaths_W2_only / county_map$population))
# min_W3 <- min(c(state_map$Deaths_W3 / state_map$population, county_map$Deaths_W3 / county_map$population))

state_map_2 <- state_map %>% 
  rename(State = "NAME") %>%
  group_by(State) %>%
  mutate(`Wave 1 Deaths per Capita` = Deaths_W1/population) %>%
  mutate(`Wave 2 Deaths per Capita` = Deaths_W2_only/population) %>%
  mutate(`Wave 3 Deaths per Capita` = Deaths_W3/population)

county_map_2 <- county_map %>% 
  rename(`Metropolitan State Area` = "NAME.y") %>%
  group_by(`Metropolitan State Area`) %>%
  mutate(`Wave 1 Deaths per Capita` = Deaths_W1/population) %>%
  mutate(`Wave 2 Deaths per Capita` = Deaths_W2_only/population) %>%
  mutate(`Wave 3 Deaths per Capita` = Deaths_W3/population)


# state and county msa

g_c_w1 <- ggplot() +
  geom_sf(data = state_map_2, aes(group = State, fill = `Wave 1 Deaths per Capita`), color = NA) +
  geom_sf(data = county_map_2, aes(group = `Metropolitan State Area`, fill = `Wave 1 Deaths per Capita`), color = NA) + # MSA Things
  scale_fill_viridis_c(name = "Deaths per Capita", option = "viridis", direction = -1) + #limits = c(min_W1, max_W1)
  labs(title = "Metropolitan State Areas, Regional Areas - Wave 1 Deaths per Capita",
       subtitle = "Until 15 June 2020\n") +
  theme_void() +
  theme(text = element_text(family = "raleway"), 
        plot.title = element_text(family = "instruction", color = "grey20"),
        axis.line = element_line(color = NA))

#
g_c_w2 <- ggplot() +
  geom_sf(data = state_map_2, aes(group = State, fill = `Wave 2 Deaths per Capita`), color = NA) + # size = 0.3
  geom_sf(data = county_map_2, aes(group = `Metropolitan State Area`, fill = `Wave 2 Deaths per Capita`), color = NA) + # MSA Things
  scale_fill_viridis_c(name = "Deaths per Capita", option = "viridis", direction = -1) +
  labs(title = "Metropolitan State Areas, Regional Areas - Wave 2 Deaths per Capita",
       subtitle = "From 15 June 2020 - 15 October 2020\n") +
  theme_void() +
  theme(text = element_text(family = "raleway"),
        plot.title = element_text(family = "instruction", color = "grey20"),
        axis.line = element_line(color = NA))

g_c_w3 <- ggplot() +
  geom_sf(data = state_map_2, aes(group = State, fill = `Wave 3 Deaths per Capita`), color = NA) +
  geom_sf(data = county_map_2, aes(group = `Metropolitan State Area`, fill = `Wave 3 Deaths per Capita`), color = NA) + # MSA Things
  scale_fill_viridis_c(name = "Deaths per Capita", option = "viridis", direction = -1) +
  labs(title = "Metropolitan State Areas, Regional Areas - Wave 3 Deaths per Capita",
       subtitle = "After 15 October 2020\n") +
  theme_void() +
  theme(text = element_text(family = "raleway"),
        plot.title = element_text(family = "instruction", color = "grey20"),
        axis.line = element_line(color = NA))



gp_c_w1 <- ggplotly(g_c_w1) %>% 
  partial_bundle()





#   geom_sf(data = state_, 
#           aes(fill = Deaths_total/population), 
#           color = NA) +  # color = NA 
#   geom_sf(data = county_map, aes(fill = Deaths_total/population), color = NA) + # MSA Things
#   scale_fill_viridis_c(name = "Deaths per Capita", option = "viridis", direction = -1) + 
#   labs(title = "Total Deaths\n") + 
#   theme_void() + 
#   theme(text = element_text(family = "raleway", size = 50))
# 
# 
# 
# transition_time(year) +
#   labs(title = "Year: {frame_time}")


# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #
# ------------------------------------------------------------------ #
# --------------------------- FUNCTIONS ---------------------------- #
# ------------------------------------------------------------------ #
# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #

# Those requiring inputs 

# ___________________________ Table of State Details  _________________________________ #

# split up to make it *hopefully* faster

detailsForecast_fun <- function(input_state) {
  
  details_forecast %>%
    filter(State %in% as.vector(input_state)) %>%
    t() # transposing for columns to be for each state   
  
}

detailsCurrent_fun <- function(input_state) {
  
  details_current %>%
    filter(State %in% as.vector(input_state)) %>%
    t() # transposing for columns to be for each state   
  
}


# ___________________________ Plot Line graph for Deaths _________________________________ #
# 

# TO DO: I could make the gDeaths functions simpler but if else has issueeeesss. 
# Or use nested function but there are also issueeeeessss.  

# Not working
# datatype <- "Daily"
# { 
#   if(input_datatype == "Daily") {
#    covid_renamed$y_val <- covid_renamed$"7-Day Average Deaths"
#   } else_if (input_datatype == "Daily per Capita") {
#     covid_renamed$y_val <- covid_renamed$"Daily Deaths per Capita"
#   } else (input_datatype == "Daily per 100k") { 
#     covid_renamed$y_val <- covid_renamed$"Daily Deaths per 100k"
#   }
# }
#   
# datatype <- c("Daily", "Daily per 100k", "Daily per Capita")
# covid_renamed$y_val <- covid_renamed$"Daily Deaths per Capita"
# 
# covid_renamed$y_val <- covid_renamed$"Daily Deaths per Capita"
# title_d <- "\nCOVID-19 Daily Deaths per Capita\n \n "
# y_d <- " \n Daily Deaths per Capita\n \n "
# 
# datatype <- "Daily Deaths per Capita"
# covid_renamed$y_val <- covid_renamed$datatype
# 
# covid_renamed$y_val <- covid_renamed$datatype
# title_d <- "\nCOVID-19 Daily Deaths per Capita\n \n "
# y_d <- " \n Daily Deaths per Capita\n \n "
# 


# gDeaths_fun <- function(input_state) {
#   
#   # if(input_datatype == "Daily") {
#   #   
#   #   covid_renamed$y_val <- covid_renamed$"7-Day Average Deaths"
#   #   title_d <- "\nCOVID-19 Daily Deaths Count\n \n "
#   #   y_d <- " \n 7-Day Average of Daily Deaths\n \n "
#   #   
#   # } else if (input_datatype == "Daily per Capita") {
#   #   
#   #   covid_renamed$y_val <- covid_renamed$"Daily Deaths per Capita"
#   #   title_d <- "\nCOVID-19 Daily Deaths per Capita\n \n "
#   #   y_d <- " \n Daily Deaths per Capita\n \n "
#   #   
#   # } else (input_datatype == "Daily per 100k") { 
#   #   
#   #   covid_renamed$y_val <- covid_renamed$"Daily Deaths per 100k"
#   #   title_d <- "\nCOVID-19 Daily Deaths per 100k\n \n "
#   #   y_d <- " \n Daily Deaths per 100k\n \n "
#   # 
#   #   }
#   
#   names(covid_renamed)
#   
# 
# covid_renamed %>% 
#   filter(`Province/State` %in% as.vector(input_state)) %>%
#   ggplot(aes(x = Date, y = y_val)) +
#   geom_rect(aes(xmin = as.Date("2020-10-01"), xmax = as.Date(max(Date)), ymin = 0, ymax = y_max), fill = "black", alpha = 0.1) +
#   # scale_fill_manual(values = "#EFEFEF", name = "Forecast") +
#   geom_line(aes(group = `Province/State`, color = `Province/State`), size = 1.1) +
#   scale_color_viridis_d(option = "viridis", name = "State") +
#   labs(title = title_d,
#        x = " \nDate\n",
#        y = y_d) +
#   scale_x_date(date_labels = "%b", 
#                date_breaks = "1 month") +
#   theme_minimal() +
#   theme(text = element_text(family = "raleway"),
#         plot.title = element_text(family = "instruction", color = "grey20", hjust = 0.5),
#         axis.line = element_line(color = "grey60"))
# #   
# }


# 3 types of Daily Deaths line plot output 
# --------------------------------------------------------------------------------------

gDeaths_daily <- function(input_state) {
  
  covid_renamed$y_val <- covid_renamed$`7-Day Average Deaths`
  title_d <- "\nCOVID-19 Daily Death Count\n \n "
  y_d <- " \n 7-Day Average Daily Deaths\n \n "
  y_max <- max(covid_renamed$y_val, na.rm = TRUE)
  
  covid_renamed %>% 
    filter(`Province/State` %in% as.vector(input_state)) %>%
    ggplot(aes(x = Date, y = y_val)) +
    geom_rect(aes(xmin = as.Date("2020-10-01"), xmax = as.Date(max(Date)), ymin = 0, ymax = y_max), fill = "black", alpha = 0.1) +
    # scale_fill_manual(values = "#EFEFEF", name = "Forecast") +
    geom_line(aes(group = `Province/State`, color = `Province/State`), size = 1.1) +
    scale_color_viridis_d(option = "viridis", name = "State") +
    labs(title = title_d,
         x = " \nDate\n",
         y = y_d) +
    scale_x_date(date_labels = "%b", 
                 date_breaks = "1 month") +
    theme_minimal() +
    theme(text = element_text(family = "raleway"),
          plot.title = element_text(family = "instruction", color = "grey20", hjust = 0.5),
          axis.line = element_line(color = "grey60"))
  
}


# gDeaths_daily("New York") + 
#   transition_reveal(Date)
# 
# # 
gDeaths_100 <- function(input_state) {

  covid_renamed$y_val <- covid_renamed$`Daily Deaths per 100k`
  title_d <- "\nCOVID-19 Daily Deaths per 100k\n \n "
  y_d <- " \n Daily Deaths per 100k\n \n "
  y_max <- max(covid_renamed$y_val, na.rm = TRUE)

  # gDeaths_fun(input_state) not working 
  
  covid_renamed %>% 
    filter(`Province/State` %in% as.vector(input_state)) %>%
    ggplot(aes(x = Date, y = y_val)) +
    geom_rect(aes(xmin = as.Date("2020-10-01"), xmax = as.Date(max(Date)), ymin = 0, ymax = y_max), fill = "black", alpha = 0.1) +
    # scale_fill_manual(values = "#EFEFEF", name = "Forecast") + # trying to add a label 
    geom_line(aes(group = `Province/State`, color = `Province/State`), size = 1.1) +
    scale_color_viridis_d(option = "viridis", name = "State") +
    labs(title = title_d,
         x = " \nDate\n",
         y = y_d) +
    scale_x_date(date_labels = "%b", 
                 date_breaks = "1 month") +
    theme_minimal() +
    theme(text = element_text(family = "raleway"),
          plot.title = element_text(family = "instruction", color = "grey20"),
          axis.line = element_line(color = "grey60"))

}
# 
gDeaths_pc <- function(input_state) {

  covid_renamed$y_val <- covid_renamed$`Daily Deaths per Capita`
  title_d <- "\nCOVID-19 Daily Deaths per Capita\n \n "
  y_d <- " \n Daily Deaths per Capita\n \n "
  y_max <- max(covid_renamed$y_val, na.rm = TRUE)

  covid_renamed %>% 
    filter(`Province/State` %in% as.vector(input_state)) %>%
    ggplot(aes(x = Date, y = y_val)) +
    geom_rect(aes(xmin = as.Date("2020-10-01"), xmax = as.Date(max(Date)), ymin = 0, ymax = y_max), fill = "black", alpha = 0.1) +
    geom_line(aes(group = `Province/State`, color = `Province/State`), size = 1.1) +
    scale_color_viridis_d(option = "viridis", name = "State") +
    labs(title = title_d,
         x = " \nDate\n",
         y = y_d) +
    scale_x_date(date_labels = "%b", 
                 date_breaks = "1 month") +
    theme_minimal() +
    theme(text = element_text(family = "raleway"),
          plot.title = element_text(family = "instruction", color = "grey20", hjust = 0.5),
          axis.line = element_line(color = "grey60"))

}


# ___________________________ Plot Line graph for Confimred Cases _________________________________ #

# gCon_fun <- function(input_state) {
#   
#   y_max <- max(covid_renamed$`7-Day Average Cases`, na.rm = TRUE)
#   
#   covid_renamed %>% 
#     filter(`Province/State` %in% as.vector(input_state)) %>%
#     ggplot(aes(x = Date, y = `7-Day Average Cases`)) +
#     geom_rect(aes(xmin = as.Date(Sys.Date()), xmax = as.Date(max(Date)), ymin = 0, ymax = y_max), fill = "black", alpha = 0.1) +
#     geom_line(aes(color = `Province/State`), size = 1.1) +
#     scale_color_viridis_d(option = "magma", name = "State") +
#     labs(title = "\nCOVID-19 Daily Confirmed Cases Count \n \n ",
#          x = " \nDate\n",
#          y = "\n7-Day Average of Confirmed Cases\n \n ") +
#     scale_x_date(date_labels = "%b", 
#                  date_breaks = "1 month") +
#     theme_minimal() +
#     theme(text = element_text(family = "raleway"),
#           plot.title = element_text(family = "instruction", color = "grey20", hjust = 0.5),
#           axis.line = element_line(color = "grey60"))
# }

# 3 types of Daily Cases line plot output 
# --------------------------------------------------------------------------------------

gCon_daily <- function(input_state) {
  
  covid_renamed$y_val <- covid_renamed$`7-Day Average Cases`
  title_d <- "\nCOVID-19 Daily Case Count\n \n "
  y_d <- " \n 7-Day Average Daily Cases\n \n "
  y_max <- max(covid_renamed$y_val, na.rm = TRUE)
  
  covid_renamed %>% 
    filter(`Province/State` %in% as.vector(input_state)) %>%
    ggplot(aes(x = Date, y = y_val)) +
    geom_rect(aes(xmin = as.Date(Sys.Date()), xmax = as.Date(max(Date)), ymin = 0, ymax = y_max), fill = "black", alpha = 0.1) +
    # scale_fill_manual(values = "#EFEFEF", name = "Forecast") +
    geom_line(aes(group = `Province/State`, color = `Province/State`), size = 1.1) +
    scale_color_viridis_d(option = "magma", name = "State") +
    labs(title = title_d,
         x = " \nDate\n",
         y = y_d) +
    scale_x_date(date_labels = "%b", 
                 date_breaks = "1 month") +
    theme_minimal() +
    theme(text = element_text(family = "raleway"),
          plot.title = element_text(family = "instruction", color = "grey20", hjust = 0.5),
          axis.line = element_line(color = "grey60"))
  
}


gCon_100 <- function(input_state) {
  
  covid_renamed$y_val <- covid_renamed$`Daily Cases per 100k`
  title_d <- "\nCOVID-19 Daily Cases per 100k\n \n "
  y_d <- " \n Daily Cases per 100k\n \n "
  y_max <- max(covid_renamed$y_val, na.rm = TRUE)
  
  # gCases_fun(input_state) not working 
  
  covid_renamed %>% 
    filter(`Province/State` %in% as.vector(input_state)) %>%
    ggplot(aes(x = Date, y = y_val)) +
    geom_rect(aes(xmin = as.Date("2020-10-01"), xmax = as.Date(max(Date)), ymin = 0, ymax = y_max), fill = "black", alpha = 0.1) +
    # scale_fill_manual(values = "#EFEFEF", name = "Forecast") + # trying to add a label 
    geom_line(aes(group = `Province/State`, color = `Province/State`), size = 1.1) +
    scale_color_viridis_d(option = "magma", name = "State") +
    labs(title = title_d,
         x = " \nDate\n",
         y = y_d) +
    scale_x_date(date_labels = "%b", 
                 date_breaks = "1 month") +
    theme_minimal() +
    theme(text = element_text(family = "raleway"),
          plot.title = element_text(family = "instruction", color = "grey20", hjust = 0.5),
          axis.line = element_line(color = "grey60"))
  
}

gCon_pc <- function(input_state) {
  
  covid_renamed$y_val <- covid_renamed$`Daily Cases per Capita`
  title_d <- "\nCOVID-19 Daily Cases per Capita\n \n "
  y_d <- " \n Daily Cases per Capita\n \n "
  y_max <- max(covid_renamed$y_val, na.rm = TRUE)
  
  covid_renamed %>% 
    filter(`Province/State` %in% as.vector(input_state)) %>%
    ggplot(aes(x = Date, y = y_val)) +
    geom_rect(aes(xmin = as.Date("2020-10-01"), xmax = as.Date(max(Date)), ymin = 0, ymax = y_max), fill = "black", alpha = 0.1) +
    # scale_fill_manual(values = "#EFEFEF", name = "Forecast") +
    geom_line(aes(group = `Province/State`, color = `Province/State`), size = 1.1) +
    scale_color_viridis_d(option = "magma", name = "State") +
    labs(title = title_d,
         x = " \nDate\n",
         y = y_d) +
    scale_x_date(date_labels = "%b", 
                 date_breaks = "1 month") +
    theme_minimal() +
    theme(text = element_text(family = "raleway"),
          plot.title = element_text(family = "instruction", color = "grey20", hjust = 0.5),
          axis.line = element_line(color = "grey60"))
  
}

# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #
# ------------------------------------------------------------------ #
# ----------------------------- CSS -------------------------------- #
# ------------------------------------------------------------------ #
# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #


# css style sheet attempt but I don't think it works
# for later

css <- HTML("

  body {

    font-family: raleway, proxima-nova, montserrat, proxima-nova-light;

  }

  h1 {

   font-family: instruction, proxima-nova-light;
   color: #777;
   text-align: center;
   line-height: 1.5;

  }

")

# source('setup.R') ----------- to add later when all these are in separate scripts


# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #
# ------------------------------------------------------------------ #
# ----------------------------- UI --------------------------------- #
# ------------------------------------------------------------------ #
# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #


# define UI - structure 
ui <- navbarPage(

    theme = shinytheme("cosmo"),
    title = "QMSS x KPMG: COVID-19 Intelligent Forecasting", 
    
    # -------------------------------------------------------------------------------------------------- #
    # ----------------------------------- First Tab - PREDICTIONS -------------------------------------- #
    # -------------------------------------------------------------------------------------------------- #
    
    tabPanel("Predictions by State",
  
              sidebarLayout( # sidebar panel and main panel
                sidebarPanel(
                  
                  h5(),
                  
                # _______________________ Select States Checkboxes Side Panel  _______________________ #
                
                  checkboxGroupInput(
                    inputId = "state", label = "Select States:",
                    choices = unique(covid_state$Province_State),
                    selected = c("New York", "Texas", "Florida"), 
                    width = "100%"),
                    width = 3,
                    
                    style = {
                      'font-family: raleway;'
                    }

                ),
          
                mainPanel(
                
                  # ____________________________ States Table Details  ____________________________ # 
                  
                  h1("State-level Details"),
                  tabsetPanel(type = "tabs",
                              
                              tabPanel("With Forecasts", 
                                       tableOutput("details_f")),
                              
                              tabPanel("Without Forecasts",
                                      tableOutput("details_c"))
                              ),
                  br(),
                  
                  
                  # _______________________ Line Plot Viz - Deaths & Cases ______________________ # 
                  
                  h1("State-level Visualization"),
                  
                  # Selection for line graph output 
                  tabsetPanel(type = "tabs",

                              # daily deaths & cases
                              tabPanel("Daily", br(),
                                       plotlyOutput("gdeaths_daily"), br(),
                                       plotlyOutput("gconfirmed_daily")),
                              
                              # daily per 100k deaths & cases
                              tabPanel("Daily per 100k", br(),
                                       plotlyOutput("gdeaths_100"), br(),
                                       plotlyOutput("gconfirmed_100")),
                              
                              # daily per capitadeaths & cases
                              tabPanel("Daily per Capita", br(),
                                       plotlyOutput("gdeaths_pc"), br(),
                                       plotlyOutput("gconfirmed_pc")), 
                  
                               br()
                  )
                )
              )
              
              # style = {
              #   'font-family: raleway;'
              # }
             
     ),
    
    # ------------------------------------------------------------------------------------------------------ #
    # ----------------------------------- Second Tab - U.S. MAPS VIZ  -------------------------------------- #
    # ------------------------------------------------------------------------------------------------------ #
        
    tabPanel("U.S. Mapping", 
             
             h1("U.S. MSA & Regional Areas COVID-19 Waves"),
             
       # sidebarLayout( # sidebar panel and main panel
       #   
       #   sidebarPanel(
       #     
       #     
       #     tabPanel(
       #       
       #       
       #     )
       #   ),
       #   
       #   mainPanel(       
       #       
           # tabsetPanel(type = "tabs",
           #         tabPanel("Wave 1", br(),
           #                  h3("Total Deaths until June 15, 2020"),
           #                  plotlyOutput("wave_1")),
           #         tabPanel("Wave 2", br(),
           #                  h3("Total Deaths from June 15, 2020 to Oct 15, 2020"),
           #                  plotlyOutput("wave_2")),
           #         tabPanel("Wave 3", br(),
           #                  h3("Total Deaths from Oct 15, 2020"),
           #                  plotlyOutput("wave_3")),
       
        br(),
       
       
          h3("Wave 1: Total Deaths from the Start until June 15, 2020"),
          plotlyOutput("wave_1"),
          br(),
          h3("Wave 2: Total Deaths from June 15, 2020 to Oct 15, 2020"),
          plotlyOutput("wave_2"),
          br(),
          h3("Wave 3: Total Deaths from Oct 15, 2020 until Now"),
          plotlyOutput("wave_3"),
          width = "80%"
          
       ),
       
            # style = {
            #   'font-family: raleway;
            # lypadding = 10px;'
            # }98
    
    # ---------------------------------------------------------------------------------------------------- #
    # ----------------------------------- Third Tab - MODEL THINGS  -------------------------------------- #
    # ---------------------------------------------------------------------------------------------------- #
    
    tabPanel("Model Analysis", h1("Model Analysis"),
             
       # sidebarLayout( # sidebar panel and main panel
       # 
       #   sidebarPanel(),
       # 
       #   mainPanel(
       tabsetPanel(type = "pills",
                   tabPanel("State Clustering",
                            h3("Categorization of States according to Spread"),
                            includeHTML(cat_d_plot), br(),
                            includeHTML(cat_c_plot)),
                   tabPanel("Tab 2"),
                   tabPanel("Tab 3")
       )
             # 
             # style = {
             #   'font-family: raleway;
             #    margin: 100;
             #   left: 50%;'
             # }
    )
  )

# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #
# ------------------------------------------------------------------ #
# ----------------------------- SERVER ----------------------------- #
# ------------------------------------------------------------------ #
# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #

# server (for outputs)
server <- function(input, output) {


  # Text of list of states that are selected
  # output$states_sel <- renderText(paste(input$state, sep = "", collapse = ", "))

  # details of states 
  output$details_f <- renderTable(detailsForecast_fun(input$state), colnames = FALSE, rownames = TRUE)
  output$details_c <- renderTable(detailsCurrent_fun(input$state), colnames = FALSE, rownames = TRUE)
  
  
  # graph of deaths
  output$gdeaths_daily <- renderPlotly(gDeaths_daily(input$state))
  output$gdeaths_100 <- renderPlotly(gDeaths_100(input$state))
  output$gdeaths_pc <- renderPlotly(gDeaths_pc(input$state))

  # graph of confirmed cases
  output$gconfirmed_daily <- renderPlotly(gCon_daily(input$state))
  output$gconfirmed_100 <- renderPlotly(gCon_100(input$state))
  output$gconfirmed_pc <- renderPlotly(gCon_pc(input$state))

  # different waves
  output$wave_1 <- renderPlotly(gp_c_w1)
  output$wave_2 <- renderPlotly(g_c_w2)
  output$wave_3 <- renderPlotly(g_c_w3)
  
  # ideally - bold first "row" / detailsFill_fun(input$state)[,1] 
  
  # getPage <- function() {
  #   return(tags$iframe(src = paste0("myhtmlfiles/", input$test, ".html"), height = "100%", width = "100%", scrolling = "yes"))
  # }
  # output$cat_deaths <- renderPlotly(cat_d_plot)

  # output$cat_deaths <- renderUI(tags$iframe(cat_d_plot))

}



# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #
# ------------------------------------------------------------------ #
# ----------------------------- RUN APP ---------------------------- #
# ------------------------------------------------------------------ #
# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #


# run the app
shinyApp(

  ui,
  server

)

