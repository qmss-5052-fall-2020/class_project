
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
library(viridis)


library(dygraphs)
library(xts)


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
font_add(family = "montserrat", regular = "fonts/Montserrat-Regular.ttf")
# font_add(family = "proxima-nova", regular = "fonts/ProximaNova-Reg.ttf")
# font_add(family = "proximanovalight", regular = "fonts/ProximaNova-Light.ttf")
# font_add(family = "montserratlight", regular = "fonts/Montserrat-Light.ttf")
# font_add(family = "montserratthin", regular = "fonts/Montserrat-Thin.ttf")

# Turn off scientific notation
options(scipen = 999)

# ------------------------  Importing Datasets -------------------------- # 

# US State Level Clean
covid_state <- read_csv("data/us_state_level_clean_2020-12-06.csv") # States only

# MSAs & Regional Areas 
covid <- read_csv("data/us_daily_msa_final_2020-12-05.csv") # includes MSAs and Regional Areas

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
  mutate("7-Day Average Deaths" = round(new_deaths_7)) %>%
  mutate("Daily Deaths per 100k" = signif(new_deaths_7/100000), 3) %>%
  mutate("Daily Deaths per Capita" = signif(covid_state$new_deaths_7/covid_state$population), 3) %>%
  rename("State" = Province_State) %>%
  mutate("7-Day Average Cases" = round(new_confirmed_7)) %>%
  mutate("Daily Cases per 100k" = signif(new_confirmed_7/100000), 3) %>%
  mutate("Daily Cases per Capita" = signif(covid_state$new_confirmed_7/covid_state$population), 3)



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

##



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

state_map_2 <- state_map %>% 
  rename(State = "NAME") %>%
  group_by(State) %>%
  mutate(`Wave 1 Deaths per Capita` = Deaths_W1/population) %>%
  mutate(`Wave 2 Deaths per Capita` = Deaths_W2_only/population) %>%
  mutate(`Wave 3 Deaths per Capita` = Deaths_W3/population) %>%
  mutate(`Wave 1 Cases per Capita` = Confirmed_W1/population) %>%
  mutate(`Wave 2 Cases per Capita` = Confirmed_W2_only/population) %>%
  mutate(`Wave 3 Cases per Capita` = Confirmed_W3/population)

county_map_2 <- county_map %>% 
  rename(`Metropolitan State Area` = "NAME.y") %>%
  group_by(`Metropolitan State Area`) %>%
  mutate(`Wave 1 Deaths per Capita` = Deaths_W1/population) %>%
  mutate(`Wave 2 Deaths per Capita` = Deaths_W2_only/population) %>%
  mutate(`Wave 3 Deaths per Capita` = Deaths_W3/population) %>%
  mutate(`Wave 1 Cases per Capita` = Confirmed_W1/population) %>%
  mutate(`Wave 2 Cases per Capita` = Confirmed_W2_only/population) %>%
  mutate(`Wave 3 Cases per Capita` = Confirmed_W3/population)


# state and county msa

gWave <- function(var_s, var_c, color_pal, title_w, legend_w) { 
  
  ggplot() + 
    geom_sf(data = state_map_2, aes(group = State, fill = var_s), color = NA) +
    geom_sf(data = county_map_2, aes(group = `Metropolitan State Area`, fill = var_c), color = NA) +
    scale_fill_viridis_c(name = legend_w, option = color_pal, direction = -1) +
    labs(title = title_w) +
    theme_void() +
    theme(text = element_text(family = "raleway"), 
          plot.title = element_text(family = "instruction", color = "grey20"),
          axis.line = element_line(color = NA))
  
}

gdw1 <- gWave(state_map_2$`Wave 1 Deaths per Capita`, 
      county_map_2$`Wave 1 Deaths per Capita`, 
      "viridis",
      "Wave 1 Deaths per Capita",
      "Deaths per Capita")

gdw2 <- gWave(state_map_2$`Wave 2 Deaths per Capita`, 
              county_map_2$`Wave 2 Deaths per Capita`, 
              "viridis",
              "Wave 2 Deaths per Capita",
              "Deaths per Capita")

gdw3 <- gWave(state_map_2$`Wave 3 Deaths per Capita`, 
              county_map_2$`Wave 3 Deaths per Capita`, 
              "viridis", 
              "Wave 3 Deaths per Capita",
              "Deaths per Capita")


gcw1 <- gWave(state_map_2$`Wave 1 Cases per Capita`, 
              county_map_2$`Wave 1 Cases per Capita`, 
              "magma",
              "Wave 1 Cases per Capita",
              "Cases per Capita")

gcw2 <- gWave(state_map_2$`Wave 2 Cases per Capita`, 
              county_map_2$`Wave 2 Cases per Capita`, 
              "magma",
              "Wave 2 Cases per Capita",
              "Cases per Capita")

gcw3 <- gWave(state_map_2$`Wave 3 Cases per Capita`, 
              county_map_2$`Wave 3 Cases per Capita`,
              "magma", 
              "Wave 3 Cases per Capita",
              "Cases per Capita")



# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #
# ------------------------------------------------------------------ #
# --------------------------- FUNCTIONS ---------------------------- #
# ------------------------------------------------------------------ #
# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #

# Those requiring inputs 

# ___________________________ Table of State Details  _________________________________ #

# split up to make it *hopefully* faster

# Include cluster data 

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


# mobility static

Date <- as.Date(unique(covid_state$Date))
Texas <- rnorm(length(Date), 30:50)
# mobility <- rep(20:35, length(date))

mobility_df <- data.frame(Date, Texas)

mobi_xts <- xts(x = mobility_df[, -1], order.by = mobility_df$Date)

mobi_g <- dygraph(mobi_xts, main = "Mobility Index") %>% 
  dyRangeSelector(height = 20) %>%
  dyHighlight(highlightCircleSize = 3, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
  dyRoller(rollPeriod = 1) %>%
  dyEvent("2020-10-15", "Today", labelLoc = "bottom") %>%
  dyShading(from = "2020-10-15", as.Date((max(mobility_df$Date)))) %>%
  dyLegend(show = "follow", hideOnMouseOut = TRUE) %>%
  dyOptions(colors = viridis(3), drawGrid = FALSE) %>%
  dyCSS("www/dygraph.css")

# ___________________________ Plot Line graph for Deaths & Cases _________________________________ #
# 

covidLineGraphs <- function(input_state, var, title, color_pal) { 
  
  covid_filt <- covid_renamed %>%
    filter(State %in% as.vector(input_state)) %>%
    select(Date, State, var) %>%
    spread(State, var)

  covid_xts <- xts(x = covid_filt[, -1], order.by = covid_filt$Date)
  
  dygraph(covid_xts, main = paste("COVID-19", title, "\n")) %>%
    dyRangeSelector(height = 20) %>%
    dyHighlight(highlightCircleSize = 3, highlightSeriesBackgroundAlpha = 0.2, hideOnMouseOut = FALSE)  %>%
    dyRoller(rollPeriod = 1) %>%
    dyEvent("2020-10-15", "Today", labelLoc = "bottom") %>%
    # dyShading(from = as.Date(Sys.Date()), as.Date((max(covid_filt$Date)))) %>%
    dyShading(from = "2020-10-15", as.Date((max(covid_filt$Date)))) %>%
    dyLegend(show = "follow", hideOnMouseOut = TRUE) %>%
    # dyOptions(colors = viridis(7)) %>%
    dyOptions(colors = color_pal, drawGrid = FALSE) %>%
    dyCSS("www/dygraph.css")

  
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
  
  fluid = TRUE,

  theme = shinytheme("cosmo"),
  title = "QMSS x KPMG: COVID-19 Intelligent Forecasting", 
    
    # -------------------------------------------------------------------------------------------------- #
    # ----------------------------------- First Tab - PREDICTIONS -------------------------------------- #
    # -------------------------------------------------------------------------------------------------- #
    
  tabPanel("Predictions by State",
             sidebarLayout( # sidebar panel and main panel
                sidebarPanel(
                  
                  h5("View COVID-19 deaths and mobility forecasts by selected state(s)"),
                  
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
                                       dygraphOutput("gdeaths_daily")), 
                                       # br(),
                                       # br(),
                                       # dygraphOutput("gconfirmed_daily")),
                              
                              # daily per 100k deaths & cases
                              tabPanel("Daily per 100k", br(),
                                       dygraphOutput("gdeaths_100")), 
                                       # br(),
                                       # br(),
                                       # dygraphOutput("gconfirmed_100")),
                              
                              # daily per capitadeaths & cases
                              tabPanel("Daily per Capita", br(),
                                       dygraphOutput("gdeaths_pc")), 
                                       # br(),
                                       # br(),
                                       # dygraphOutput("gconfirmed_pc")), 
                  
                               br()
                  ),
                  br(),
                  h1("Mobility Index"),
                  dygraphOutput("mobility_g"),
                  br()
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
             
        h1("U.S. Metropolitan State Areas & Regional Areas COVID-19 Waves"),

       br(),
       
       # h3("Wave 1 - From the beginning until 15 June 2020"), 
       # fluidRow(
       #   splitLayout(
       #     plotlyOutput("wave_1_d") 
       #     # plotlyOutput("wave_1_c")
       #     )
       #   ),
       # br(),
       # h3("Wave 2 - From 15 June 2020 until 15 Oct 2020"), 
       # fluidRow(
       #   splitLayout(
       #     plotlyOutput("wave_2_d")
       #     # plotlyOutput("wave_2_c")
       #   )
       # ),
       # br(),
       # h3("Wave 3 - From 15 June 2020 until now"), 
       # fluidRow(
       #   splitLayout(
       #     plotlyOutput("wave_3_d")
       #     # plotlyOutput("wave_3_c")
       #   )
       # )
       
       h3("Wave 1 - From the beginning until 15 June 2020"), 
       plotlyOutput("wave_1_d"),
       br(),
       h3("Wave 2 - From 15 June 2020 until 15 Oct 2020"),
       plotlyOutput("wave_2_d"),
       br(),
       h3("Wave 3 - From 15 June 2020 until now"),
       plotlyOutput("wave_3_d"),
       br()
       
       
     ),
    # ---------------------------------------------------------------------------------------------------- #
    # ----------------------------------- Third Tab - MODEL THINGS  -------------------------------------- #
    # ---------------------------------------------------------------------------------------------------- #
    
    tabPanel("About the Model", h1("Model Information"),
             
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
  output$gdeaths_daily <- renderDygraph(covidLineGraphs(input$state, 
                                                        "7-Day Average Deaths", 
                                                        "Daily Death Count",
                                                        viridis(7)))
  output$gdeaths_100 <- renderDygraph(covidLineGraphs(input$state, 
                                                      "Daily Deaths per 100k", 
                                                      "Daily Deaths per 100k",
                                                      viridis(7)))
  output$gdeaths_pc <- renderDygraph(covidLineGraphs(input$state, 
                                                     "Daily Deaths per Capita", 
                                                     "Daily Deaths per Capita",
                                                     viridis(7)))

  # # graph of confirmed cases
  # output$gconfirmed_daily <- renderDygraph(covidLineGraphs(input$state, 
  #                                                          "7-Day Average Cases", 
  #                                                          "Daily Cases Count",
  #                                                          inferno(7)))
  # output$gconfirmed_100 <- renderDygraph(covidLineGraphs(input$state, 
  #                                                        "Daily Cases per 100k", 
  #                                                        "Daily Cases per 100k",
  #                                                        inferno(7)))
  # output$gconfirmed_pc <- renderDygraph(covidLineGraphs(input$state, 
  #                                                       "Daily Cases per Capita", 
  #                                                       "Daily Cases per Capita",
  #                                                       inferno(7)))

  
  output$mobility_g <- renderDygraph(mobi_g)
  # different waves
  output$wave_1_d <- renderPlotly(gdw1)
  output$wave_2_d <- renderPlotly(gdw2)
  output$wave_3_d <- renderPlotly(gdw3)
  
  output$wave_1_c <- renderPlotly(gcw1)
  output$wave_2_c <- renderPlotly(gcw2)
  output$wave_3_c <- renderPlotly(gcw3)
  
}



# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #
# -----------------------------------------            ------------------------- #
# ----------------------------- RUN APP ---------------------------- #
# ------------------------------------------------------------------ #
# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #


# run the app
shinyApp(

  ui,
  server

)

