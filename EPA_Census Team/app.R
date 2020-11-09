# 1 load libraries ----
library(shiny)
library(shinydashboard)
library(tidyverse)

# 2 run scripts ----
# * 2.1 Housing (Asahi) --------------------------------------------------------
source("Asahi_housing/housing.r")
source("Asahi_housing/cost_income.R")
# * 2.2 Sociodemographic (Alison) --------------------------------------------------------
source("viz_scripts/alison_sociodem.R")
# * 2.3 Demographics (Gretchen) --------------------------------------------------------
source("viz_scripts/gretchen_demo.R")
# * 2.4 Walkability (Gretchen) --------------------------------------------------------
source("viz_scripts/gretchen_walk.R")
# * 2.5 Business/Industry (Kyung) --------------------------------------------------------
source("viz_scripts/kyung_business.R")
# * 2.6 Broadband (Gretchen) --------------------------------------------------------
source("viz_scripts/gretchen_bband.R")
# * 2.7 Households (Alison) --------------------------------------------------------
source("viz_scripts/alison_childcare.R")
# * 2.8 Education + Earnings (Alisha) --------------------------------------------------------
source("viz_scripts/alisha_earn.R")

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Columbia TOP Tool"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics",
               tabName = "demo_tab",
               icon = icon("dashboard")),
      menuItem("Childcare",
               tabName = "child_tab",
               icon = icon("dashboard")),
      menuItem("Quality of Life",
               tabName = "qol_tab",
               icon = icon("dashboard")),
      menuItem("Housing",
               tabName = "housing_tab",
               icon = icon("dashboard")),
      menuItem("Industry",
               tabName = "industry_tab",
               icon = icon("dashboard")),
      menuItem("Broadband",
               tabName = "bband_tab",
               icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    fluidRow(
        box(textInput(inputId = "location", label = "Location", value = NULL, placeholder = "Enter location")
      )
    ),
    fluidRow(
      tabItems(
        tabItem(tabName = "demo_tab",
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("gender", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("age", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("race", height = "300px")
                )
                ),
        tabItem(tabName = "child_tab",
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("household", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("children", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("grandchildren", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("grandparent", height = "300px")
                )
                ),
        tabItem(tabName = "qol_tab",
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("comparison", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("walkMI", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  leafletOutput("walkmap", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  leafletOutput("walkmap2", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  leafletOutput("walkmap3", height = "300px")
                )
                ),
        tabItem(tabName = "housing_tab",
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("rent", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("income_rent", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("mobility_1", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotOutput("mobility_2", height = "300px")
                )
                ),
        tabItem(tabName = "industry_tab",
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("industry", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("quotient", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("payroll", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("diff_employ", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("employ", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  plotlyOutput("earn", height = "300px")
                )
                ),
        tabItem(tabName = "bband_tab",
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  leafletOutput("bband", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  leafletOutput("bband2", height = "300px")
                ),
                box(
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  leafletOutput("bband3", height = "300px")
                )
        )
      )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Demographics
  output$gender <- renderPlot({genderpie})
  output$age <- renderPlot({agebar})
  output$race <- renderPlot({racebar})
  # Childcare
  output$household <- renderPlotly({household_plotly})
  output$children <- renderPlotly({children_plotly})
  output$grandchildren <- renderPlotly({grandchildren_plotly})
  output$grandparent <- renderPlotly({grandparents_plotly})
  # Quality of Life
  output$walkmap <- renderLeaflet({walkmap})
  output$walkmap2 <- renderLeaflet({walkmap2})
  output$walkmap3 <- renderLeaflet({walkmap3})
  output$comparison <- renderPlot({comparison_plot})
  output$walkMI <- renderPlotly({plotc_plotly})
  # Hounsing
  output$rent <- renderPlot({income_perc_plot})
  output$income_rent <- renderPlotly({plotly_income_rent})
  output$mobility_1 <- renderPlot({p1_mobility})
  output$mobility_2 <- renderPlot({p2_mobility})
  # Industry
  output$industry <- renderPlotly({est_ind_plotly})
  output$quotient <- renderPlotly({quotient_plotly})
  output$payroll <- renderPlotly({payroll_plotly})
  output$diff_employ <- renderPlotly({diff_plotly})
  output$employ <- renderPlotly({employee_plotly})
  output$earn <- renderPlot({earn_plot})
  # Broadband
  output$bband <- renderLeaflet({bbandmap})
  output$bband2 <- renderLeaflet({bbandmap2})
  output$bband3 <- renderLeaflet({bbandmap3})
}

# Run the application 
shinyApp(ui = ui, server = server)

