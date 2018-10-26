library(databrew)
library(shiny)
library(shinydashboard)
header <- dashboardHeader(title="Nepal Data Hub")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text = 'Flight planner',
      tabName = 'flight_planner',
      icon = icon('plane'))
  )
)

body <- dashboardBody(
  tabItems(
   
    tabItem(tabName = 'flight_planner',
            fluidPage())
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
}

shinyApp(ui, server)