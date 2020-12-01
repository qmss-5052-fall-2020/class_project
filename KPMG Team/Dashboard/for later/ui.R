library(shiny)
library(dplyr)

source('setup.R')

shinyUI(
  fluidPage(

    titlePanel("COVID-19 Intelligent Forecasting"),

    sidebarLayout(
      sidebarPanel(title="Make selections",
                   selectInput("Province_State", label = "State",
                               lookup_state,
                               selected = "New York")
      ),

      mainPanel(h2("Visualization"),

                g_deaths

                )


    )


    # tabsetPanel(
    #
    #   tabPanel(
    #     "Controls",
    #     selectInput(
    #       'indicator',
    #       'COVID-19',
    #       lookup_list2,
    #       selected = 'Fertility rate, total (births per woman)'
    #     ),
    #
    #   selectInput(
    #     "ctry",
    #     label = "Country",
    #     lookup_list_countries,
    #     selected = "US"
    #     )
    #   ),
    #
    # tabPanel(
    #   "Chart",
    #   p("Select a country for an interactive chart"),
    #   dygraphOutput("dchart", width = "100%")
    #   )

    )
  )
)

