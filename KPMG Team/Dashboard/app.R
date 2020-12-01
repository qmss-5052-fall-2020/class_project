
# ------------------------------------------------------------------------------------ #
# ************************************************************************************ #

# COVID-19 Intelligent Forecasting Dashboard Functions
# KPMG x QMSS Practicum 2020
# Team: Ariel Luo, Sydney Bolim Son, Andrew Thvedt, Sherry Huang, Jen Woo, Louisa Ong

# Dashboard Lead: Louisa Ong

# ------------------------------------------------------------------------------------ #
# ************************************************************************************ #


# rShiny dashboard app app.R file

library(shiny)
library(shinythemes)
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
font_add(family = "montserrat", regular = "fonts/Montserrat-Regular.ttf")
font_add(family = "proxima-nova", regular = "fonts/ProximaNova-Reg.ttf")
font_add(family = "proximanovalight", regular = "fonts/ProximaNova-Light.ttf")
# font_add(family = "montserratlight", regular = "fonts/Montserrat-Light.ttf")
# font_add(family = "montserratthin", regular = "fonts/Montserrat-Thin.ttf")

# Turn off scientific notation
options(scipen = 999)

# import latest csv with updated predictions
covid <- read_csv("data/us_state_level_clean_2020-11-29.csv")


details <- covid %>%
  filter(Date == as.Date(max(Date))) %>%
  mutate("Total Deaths per Capita" = signif(Deaths/population, 3)) %>%
  mutate("Total Cases per Capita" = signif(Confirmed/population, 3)) %>%
  select(Province_State, population, Deaths, Confirmed,
         `Total Deaths per Capita`, `Total Cases per Capita`, stringency) %>%
  rename("State" = Province_State) %>%
  rename("Stringency Index" = stringency) %>%
  # rename("Containment Index" = containment) %>%
  rename("Population" = population) %>%
  rename("Total Deaths" = Deaths) %>%
  rename("Total Cases" = Confirmed) %>%
  mutate("Spread Category" = "Semi-Controlled") %>% # arbitrary
  mutate("Mobility Index" = -22)


# ------------------------------------------------------------------ #
# --------------------------- FUNCTIONS ---------------------------- #
# ------------------------------------------------------------------ #


gDeaths_fun <- function(input_state) {

  # filtering dataframe for states
  covid %>%
    filter(Province_State %in% as.vector(input_state)) %>%
    ggplot(aes(x = Date, y = new_deaths_7,
             group = Province_State, color = Province_State)) +
    geom_line(size = 1.1) +
    scale_color_viridis_d(option = "viridis", name = "State") +
    geom_vline(xintercept = as.Date("2020-10-01"), color = "slategray", size = 2, alpha = 0.3) +
    # geom_rect(aes(xmin = as.Date("2020-10-01"), xmax = max(Date), ymin = 0, ymax = max(new_deaths_7)), alpha = 0.1, inherit_aes = FALSE) +
    labs(title = "\nCOVID-19 Daily Deaths Count\n",
         x = "\nDate\n",
         y = "\n7-Day Average of Daily Deaths\n") +
    theme_minimal() +
    theme(text = element_text(family = "raleway"),
          plot.title = element_text(family = "instruction", color = "grey20"),
          axis.line = element_line(color = "grey60"))

  }

gCon_fun <- function(input_state) {

  covid %>%
    filter(Province_State %in% as.vector(input_state)) %>%
    ggplot(aes(x = Date, y = new_confirmed_7,
               group = Province_State, color = Province_State)) +
    geom_line(size = 1.1) +
    scale_color_viridis_d(option = "magma", name = "State") +
    geom_vline(xintercept = as.Date(Sys.Date()), color = "slategray", size = 2, alpha = 0.3) +
    labs(title = "\nCOVID-19 Daily Confirmed Cases Count\n",
         x = "\nDate\n",
         y = "\n7-Day Average of Confirmed Cases\n") +
    theme_minimal() +
    theme(text = element_text(family = "raleway"),
          plot.title = element_text(family = "instruction", color = "grey20"),
          axis.line = element_line(color = "grey60"))
}


gDeaths_fun(c("New York", "Florida", "California"))

detailsFill_fun <- function(input_state) {

  details_fil <- details %>%
    filter(State %in% as.vector(input_state))

  details_tbl <- gather(details_fil[1,]) %>%
    left_join(gather(details_fil[2,]), by = "key") %>%
    left_join(gather(details_fil[3,]), by = "key") %>%
    left_join(gather(details_fil[4,]), by = "key") %>%
    left_join(gather(details_fil[5,]), by = "key") %>%
    left_join(gather(details_fil[6,]), by = "key") %>%
    left_join(gather(details_fil[7,]), by = "key") %>%
    left_join(gather(details_fil[8,]), by = "key") %>%
    left_join(gather(details_fil[9,]), by = "key") %>%
    left_join(gather(details_fil[10,]), by = "key") %>%
    left_join(gather(details_fil[11,]), by = "key") %>%
    left_join(gather(details_fil[12,]), by = "key") %>%
    left_join(gather(details_fil[13,]), by = "key") %>%
    left_join(gather(details_fil[14,]), by = "key") %>%
    left_join(gather(details_fil[15,]), by = "key") %>%
    left_join(gather(details_fil[16,]), by = "key") %>%
    left_join(gather(details_fil[17,]), by = "key") %>%
    left_join(gather(details_fil[18,]), by = "key") %>%
    left_join(gather(details_fil[19,]), by = "key") %>%
    left_join(gather(details_fil[20,]), by = "key") %>%
    left_join(gather(details_fil[21,]), by = "key") %>%
    left_join(gather(details_fil[22,]), by = "key") %>%
    left_join(gather(details_fil[23,]), by = "key") %>%
    left_join(gather(details_fil[24,]), by = "key") %>%
    left_join(gather(details_fil[25,]), by = "key") %>%
    left_join(gather(details_fil[26,]), by = "key") %>%
    left_join(gather(details_fil[27,]), by = "key") %>%
    left_join(gather(details_fil[28,]), by = "key") %>%
    left_join(gather(details_fil[29,]), by = "key") %>%
    left_join(gather(details_fil[30,]), by = "key") %>%
    left_join(gather(details_fil[31,]), by = "key") %>%
    left_join(gather(details_fil[32,]), by = "key") %>%
    left_join(gather(details_fil[33,]), by = "key") %>%
    left_join(gather(details_fil[34,]), by = "key") %>%
    left_join(gather(details_fil[35,]), by = "key") %>%
    left_join(gather(details_fil[36,]), by = "key") %>%
    left_join(gather(details_fil[37,]), by = "key") %>%
    left_join(gather(details_fil[38,]), by = "key") %>%
    left_join(gather(details_fil[39,]), by = "key") %>%
    left_join(gather(details_fil[40,]), by = "key") %>%
    left_join(gather(details_fil[41,]), by = "key") %>%
    left_join(gather(details_fil[42,]), by = "key") %>%
    left_join(gather(details_fil[43,]), by = "key") %>%
    left_join(gather(details_fil[44,]), by = "key") %>%
    left_join(gather(details_fil[45,]), by = "key") %>%
    left_join(gather(details_fil[46,]), by = "key") %>%
    left_join(gather(details_fil[47,]), by = "key") %>%
    left_join(gather(details_fil[48,]), by = "key") %>%
    left_join(gather(details_fil[49,]), by = "key") %>%
    left_join(gather(details_fil[50,]), by = "key") %>%
    left_join(gather(details_fil[51,]), by = "key") %>%
    remove_empty(which = "cols")

  return(details_tbl)

}


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

# source('setup.R')

# define UI

ui <- fluidPage(

    theme = shinytheme("cosmo"),

    title = "COVID-19 Forecasting",

    titlePanel(h1("QMSS x KPMG: COVID-19 INTELLIGENT FORECASTING", align = "center", style = {'line-height: 4; font-family: raleway;'})),

              # style = "font-family: 'Raleway', sans-serif;
               # font-weight: 500;
               # line-height: 2;
               # color: #777;"),
    sidebarLayout(

      sidebarPanel(
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

        tabPanel("Details", tableOutput("details")),

        tabPanel("Deaths Count",
                 plotlyOutput("gdeaths")),

        tabPanel("Confirmed Cases Count",
                 plotlyOutput("gconfirmed")),

        style = {
          'font-family: raleway;'
        }

      )

    )

)

server <- function(input, output) {


  # list of states that are selected
  output$states_sel <- renderText(paste(input$state, sep = "", collapse = ", "))

  # graph of deaths
  output$gdeaths <- renderPlotly(gDeaths_fun(input$state))

  # graph of confirmed cases
  output$gconfirmed <- renderPlotly(gCon_fun(input$state))

  output$details <- renderTable(detailsFill_fun(input$state), colnames = FALSE)


}


# run the app
shinyApp(

  ui,
  server

)
