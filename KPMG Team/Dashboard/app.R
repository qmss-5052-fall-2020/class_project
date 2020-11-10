
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


# define UI 

ui <- pageWithSidebar(
  
  # App title 
  headerPanel("COVID-19 Intelligent Forecasting"), 
  
  # Sidebar panel for inputs 
  sidebarPanel(), 
  
  # Main panel for displaying outputs 
  mainPanel()
  
)


server <- function(input, output) {
  
}


shinyApp(ui, server)


runApp()
