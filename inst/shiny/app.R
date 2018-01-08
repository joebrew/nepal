library(shiny)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)
library(leaflet)
library(nepal)

header <- dashboardHeader(title="Nepal Data Hub")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text="Main",
      tabName="main",
      icon=icon("database")),
    menuItem(
      text="Atlas",
      tabName="atlas",
      icon=icon("map"),
      menuItem(text = 'Elevation', 
                  tabName = 'elevation', 
                  href = NULL, 
                  newtab = TRUE,
                  icon = shiny::icon("caret-square-o-up"), 
                  selected = NULL),
      menuItem(text = 'Population density', 
               tabName = 'population_density', 
               href = NULL, 
               newtab = TRUE,
               icon = shiny::icon("caret-square-o-up"), 
               selected = NULL)
      ),
    menuItem(
      text="Demography",
      tabName="demography",
      icon=icon("address-card")),
    menuItem(
      text="Health",
      tabName="health",
      icon=icon("stethoscope")),
    menuItem(
      text="Economy",
      tabName="economy",
      icon=icon("money")),
    menuItem(
      text = 'About',
      tabName = 'about',
      icon = icon("cog", lib = "glyphicon"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(
      tabName="main",
      fluidPage(
        fluidRow(
          h1('The Nepal Data Hub', align = 'center'),
          h5("An initiative of Stony Brook University's Global Health Institute", align = 'center'),
          h3('Harnessing data to power research'),
          p('The Nepal Data Hub aims to bring diverse datasets pertaining to Nepal under one roof, making data available for research and analysis.')
        ),
        fluidRow(
          column(6,
                 leafletOutput('leaf',
                               height= "250px")),
          column(6,
                 leafletOutput('leaf_cities',
                               height= "250px"))
        ),
        br(),
        fluidRow(
          column(6,
                 leafletOutput('leaf_elevation',
                               height = '250px')),
          column(6,
                 leafletOutput('leaf_misc',
                               height = '250px'))
        )
      )
    ),
    tabItem(
      tabName="atlas",
      fluidPage(
        h3('Atlas')
      )
    ),
    tabItem(
      tabName="elevation",
      fluidPage(
        h3('Elevation'),
        p('Under construction')
      )
    ),
    tabItem(
      tabName="major_cities",
      fluidPage(
        h3('Major cities'),
        p('Under construction')
      )
    ),
    tabItem(
      tabName="population_density",
      fluidPage(
        h3('Population density'),
        p('Under construction')
      )
    ),
    tabItem(
      tabName="demography",
      fluidPage(
        h3('Demography'),
        p('Under construction.')
      )
    ),
    tabItem(
      tabName="health",
      fluidPage(
        h3('Health'),
        p('Under construction.')
      )
    ),
    tabItem(
      tabName="economy",
      fluidPage(
        h3('Economy'),
        p('Under construction.')
      )
    ),
    tabItem(
      tabName = 'about',
      fluidPage(
        fluidRow(
          valueBox(value = 'The goal',
                   subtitle = 'Apply the best in technology to drastically improve TB case detection and treatment through active case findinginding, drone transport of specimens and supplies, and rapid molecular diagnostics.',
                   color = 'blue',
                   icon = icon("location-arrow")),
          valueBox(value = 'The team',
                          subtitle = 'Peter Small, Simon Grandjean Lapierre, Astrid Knoblauch, Kunchok Dorjee, Joe Brew, Jesse McKinney, Liana Langdon-Embry, Emile Redwood, Annabelle Jones, Benjamin Schwarz',
                          color = 'blue',
                   icon = icon("group")),
                 valueBox(value = 'The tech',
                          subtitle = 'Instructional videos on mobile phones, Medication reminders/recorders, Remote symptom monitor, Drones for transport of specimens/supplies',
                          color = 'blue',
                          icon = icon("microchip"))),
        fluidRow(
          h3('Want to contribute?'),
          p('The NDH is an entirely transparent, open-source project. The underlying data and documentation (in the form of an R package) are hosted at ',
            a(href = 'https://github.com/joebrew/nepal', 'on github'),
            ', the application can be downloaded and run locally, and pull requests are more than welcome.')
        ),
        fluidRow(
          div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
                 h4('This web application was built in partnership with ',
                   a(href = 'http://databrew.cc',
                     target='_blank', 'Databrew'),
                   align = 'center'),
          p('Empowering research and analysis through collaborative data science.', align = 'center'),
          div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                             icon = icon("envelope", lib = "font-awesome")),
                href="mailto:info@databrew.cc",
                align = 'center')), 
          style = 'text-align:center;'
          )
        )
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
  output$leaf <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('OpenTopoMap') %>%
      addPolylines(data = nepal::nep0)
  })
  output$leaf_cities <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('Thunderforest.TransportDark') %>%
      addCircleMarkers(data = nepal::cities,
                 ~lng,
                 ~lat,
                 color = 'orange',
                 radius = ~sqrt(nepal::cities_sp@data$pop / 1000),
                 popup = paste0(nepal::cities_sp@data$city,
                                ', ',
                                nepal::cities_sp@data$province,
                                ', Population: ',
                                nepal::cities_sp@data$pop)) 
  })
  output$leaf_elevation <- renderLeaflet({
    pal <- colorNumeric(c('darkgreen', 'red', 'yellow'), values(elevation_raster_small),
                        na.color = "transparent")
    
    leaflet() %>% 
      addTiles() %>%
      # addProviderTiles('Esri.NatGeoWorldMap') %>%
      addRasterImage(elevation_raster_small, colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = values(elevation_raster_small),
                title = "Elevation")
  })
  output$leaf_misc <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('CartoDB.DarkMatter') %>%
      addPolylines(data = nepal::nep0,
                   color = 'red',
                   weight = 2) %>%
      addPolygons(data = nepal::nep2,
                   color = 'yellow',
                   weight = 0.6,
                  fillOpacity = 0,
                  popup = nepal::nep2@data$NAME_2) %>%
      addPolylines(data = nepal::nep1,
                  color = 'red',
                  weight = 0.5)
  })

}

shinyApp(ui, server)