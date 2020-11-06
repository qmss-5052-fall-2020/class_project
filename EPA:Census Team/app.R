library(shiny)
library(shinydashboard)
library(tidyverse)

source("housing.r")
source("cost_income.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Columbia TOP Tool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Planner",
               tabName = "planner_tab",
               icon = icon("dashboard")),
      menuItem("Developer",
               tabName = "developer_tab",
               icon = icon("dashboard")),
      menuItem("Prospective Resident",
               tabName = "prospective_resident_tab",
               icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    fluidRow(
      tabItems(
        tabItem(tabName = "planner_tab",
                box(textInput(inputId = "location", label = "Location", value = NULL, placeholder = "Enter location"))),
        tabItem(tabName = "developer_tab"),
        tabItem(tabName = "prospective_resident_tab")
      )
    ),
    fluidRow( 
      box(
        title = "Rent",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotOutput("rent", height = "300px")
      ),
      box(
        title = "Income/Rent",
        solidHeader = TRUE,
        collapsible = TRUE,
        plotlyOutput("income_rent", height = "300px")
      )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$rent<- renderPlot({income_perc_plot})
  output$income_rent<- renderPlotly({plotly_income_rent})
}

# Run the application 
shinyApp(ui = ui, server = server)

