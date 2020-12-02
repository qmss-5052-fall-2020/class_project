
# ------------------------------------------------------------------------------------ #
# ************************************************************************************ #

# COVID-19 Intelligent Forecasting Dashboard Functions
# KPMG x QMSS Practicum 2020
# Team: Ariel Luo, Sydney Bolim Son, Andrew Thvedt, Sherry Huang, Jen Woo, Louisa Ong

# Visualizations Lead: Louisa Ong

# ------------------------------------------------------------------------------------ #
# ************************************************************************************ #



# rShiny dashboard app app.R file

# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #
# ------------------------------------------------------------------ #
# ---------------------------- SETUP ------------------------------- #
# ------------------------------------------------------------------ #
# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #


library(shiny)
library(shinythemes)
library(shinydashboard)
library(plotly)
library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(showtext)
library(janitor)


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

# import latest csv with updated predictions
# TO DO: use Sydney's branch on Andrew's Github
covid <- read_csv("data/us_state_level_clean_2020-11-29.csv")


# rename for State details table (mostly totals)
# also selecting only variables to be displayed 
details <- covid %>%
  filter(Date == as.Date(max(Date))) %>%
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

# Renaming so that the variable names in plotly shows up well
covid_renamed <- covid %>% 
  mutate("7-Day Average Deaths" = new_deaths_7) %>%
  mutate("Daily Deaths per 100k" = new_deaths_7/100000) %>%
  mutate("Daily Deaths per Capita" = covid$new_deaths_7/covid$population) %>%
  rename("Province/State" = Province_State) %>%
  mutate("7-Day Average Cases" = new_confirmed_7) %>%
  mutate("Daily Cases per 100k" = new_confirmed_7/100000) %>%
  mutate("Daily Cases per Capita" = covid$new_confirmed_7/covid$population)


# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #
# ------------------------------------------------------------------ #
# --------------------------- FUNCTIONS ---------------------------- #
# ------------------------------------------------------------------ #
# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #

# ___________________________ Table of State Details  _________________________________ #

detailsFill_fun <- function(input_state) {
  
  details %>%
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

# 
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
          plot.title = element_text(family = "instruction", color = "grey20", hjust = 0.5),
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
                  
                  h3(),
                  
                # ____________________________ Select States Checkboxes Side Panel  ____________________________ #
                
                  checkboxGroupInput(
                    inputId = "state", label = "Select States:",
                    choices = unique(covid$Province_State),
                    selected = c("New York", "Texas", "Florida"), width = "100%"),
                  width = 3,
                  
                  style = {
                    'font-family: raleway;'
                  }
                  
                ),
          
                mainPanel(
                
                  # ____________________________ States Table Details  ____________________________ # 
                  
                  h1("State-level Details"),
                  tableOutput("details"),
                  br(),
                  
                  
                  # ____________________________ Line Plot Viz - Deaths & Cases ____________________________ # 
                  
                  h1("State-level Visualization"),
                  
                  # Selection for line graph output 
                  tabsetPanel(type = "tabs",
                              
                              # daily deaths & cases
                              tabPanel("Daily", 
                                       plotlyOutput("gdeaths_daily"), br(),
                                       plotlyOutput("gconfirmed_daily")),
                              
                              # daily per 100k deaths & cases
                              tabPanel("Daily per 100k",
                                       plotlyOutput("gdeaths_100"), br(),
                                      plotlyOutput("gconfirmed_100")),
                              
                              # daily per capitadeaths & cases
                              tabPanel("Daily per Capita",
                                       plotlyOutput("gdeaths_pc"), br(),
                                       plotlyOutput("gconfirmed_pc")), 
                  
                  # # Line graphs for deaths and confirmed cases 
                  # plotlyOutput("gdeaths"),
                  # plotlyOutput("gconfirmed"),
                  
                               br()
                  )
                )
              ),
              
              style = {
                'font-family: raleway;'
              }
             
     ),
    
    # ------------------------------------------------------------------------------------------------------ #
    # ----------------------------------- Second Tab - U.S. MAPS VIZ  -------------------------------------- #
    # ------------------------------------------------------------------------------------------------------ #
        
    tabPanel("U.S. Mapping", h2("U.S. Mapping"),
             
       tabsetPanel(type = "tabs",
                   tabPanel("Daily Figures"),
                   tabPanel("Daily per Capita"),
                   tabPanel("Daily per 100k")
       ),
       
      style = {
        'font-family: raleway;'
      }
    ),
    
    # ---------------------------------------------------------------------------------------------------- #
    # ----------------------------------- Third Tab - MODEL THINGS  -------------------------------------- #
    # ---------------------------------------------------------------------------------------------------- #
    
    tabPanel("Model Analysis", h2("Model Analysis"),
             
             tabsetPanel(type = "pills",
                         tabPanel("Tab 1"),
                         tabPanel("Tab 2"),
                         tabPanel("Tab 3")
             ),
             
             style = {
               'font-family: raleway;'
             }
    )
  )

# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #
# ------------------------------------------------------------------ #
# ----------------------------- SERVER ----------------------------- #
# ------------------------------------------------------------------ #
# \/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/ #

# server (for outputs)
server <- function(input, output) {


  # list of states that are selected
  # output$states_sel <- renderText(paste(input$state, sep = "", collapse = ", "))

  # graph of deaths
  output$gdeaths_daily <- renderPlotly(gDeaths_daily(input$state))
  output$gdeaths_100 <- renderPlotly(gDeaths_100(input$state))
  output$gdeaths_pc <- renderPlotly(gDeaths_pc(input$state))

  # graph of confirmed cases
  output$gconfirmed_daily <- renderPlotly(gCon_daily(input$state))
  output$gconfirmed_100 <- renderPlotly(gCon_100(input$state))
  output$gconfirmed_pc <- renderPlotly(gCon_pc(input$state))

  output$details <- renderTable(detailsFill_fun(input$state), colnames = FALSE, rownames = TRUE)
  # idaelly - bold first "row" / detailsFill_fun(input$state)[,1] 


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

