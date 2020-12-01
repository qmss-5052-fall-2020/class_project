library(shiny)
library(tidyverse)
library(dygraphs)

source('setup.R')


shinyServer(function(input, output, session) {

  # Build reactive expression that takes values from the input to fetch the data
  # state <- reactive({
  #   merge_to_wdi(input$state)
  # })
  checkboxGroupInput(
    "state", label = "States:", choices = unique(covid$Province_State), selected = "New York",
     width = "100%")

  output$plot_d <- renderPlotly({

    covid_sel <- covid_data %>%
      filter(Province_State %in% state)

    ggplotly(g_deaths)

  })

})

unique(covid$Province_State)
