library(shiny)
library(shinydashboard)
library(dplyr)
library(leaflet)
library(nepal)
library(raster)
library(ggplot2)
library(ggrepel)
library(DT)
library(RColorBrewer)
library(databrew)
library(geosphere)
source('global.R')
header <- dashboardHeader(title="Nepal Data Hub")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text = 'Timeline',
      tabName = 'timeline',
      icon = icon('calendar')
    ),
    menuItem(
      text = 'Flight planner',
      tabName = 'flight_planner',
      icon = icon('plane')),
    menuItem(
      text="Main",
      tabName="main",
      icon=icon("database")),
    menuItem(
      text = 'Pyuthan',
      tabName = 'pyuthan',
      icon = icon('map')
    ),
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
          # h1(icon('line-chart'), align = 'center'),
          h1('The Nepal Data Hub', align = 'center'),
          h5("An initiative of Stony Brook University's Global Health Institute", align = 'center'),
          h1(icon('sitemap'), align = 'center'),
          h3('Harnessing data to power insight'),
          p('The Nepal Data Hub brings diverse datasets pertaining to Nepal into one integrated data platform, making data available for research. Through integration, the NDH aims to streamline analyses, improve reproducibility, and guide policy through insight, rigor, and transparency.')
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
      tabName = 'timeline',
      fluidPage(
        fluidRow(
          # h1(icon('line-chart'), align = 'center'),
          h1('DrOTS Nepal timeline', align = 'center')
        ),
        fluidRow(
          column(4,
                 checkboxGroupInput('filter_groups',
                                    'Filter organizations',
                                    choices = c('BNMT','SBU','NFL', 'Other'),
                                    selected = c('BNMT','SBU','NFL', 'Other'))),
          column(4,
                 checkboxGroupInput('filter_types',
                                    'Filter categories',
                                    choices = c('Events' = 'Event',
                                                'Activities' = 'Activity',
                                                'Reports' = 'Report'),
                                    selected = c('Event',
                                                 'Activity',
                                                 'Report'))),
          column(4,
                 textInput('filter_people',
                           'Filter people',
                            value = ''))
        ),
        fluidRow(
          textOutput('tv_text')
        ),
        fluidRow(
          timevisOutput('tv')
        ),
        h3('Raw data'),
        fluidRow(
          DT::dataTableOutput('tv_table')
        ),
        br()
      )
    ),
    tabItem(
      tabName = 'pyuthan',
      fluidPage(
        fluidRow(
          column(6,
                 h2('Elevation'),
                 leafletOutput('pyuthan_elevation')),
          column(6,
                 h2('Elevation'),
                 leafletOutput('pyuthan_elevation_rough'))
        ),
        fluidRow(
          column(6,
                 h2('Population density'),
                 leafletOutput('pyuthan_population'))
        )
      )
    ),
    tabItem(tabName = 'flight_planner',
            fluidPage(
              fluidRow(column(4,
                              h1('Flight planner')),
                       column(6,
                              selectInput('country',
                                          'Country',
                                          choices = c('Nepal', 'Madagascar')))),
                               fluidPage(
                                 column(8,
                                        h3('Map tool'),
                                        leafletOutput('flights')),
                                 column(4,
                                        uiOutput('geo_coordinates_ui'))
                               ),
                               fluidRow(column(6,
                                               h3('Elevation profile'),
                                               plotOutput('flight_path_elevation_plot')),
                                        column(6,
                                               h3('Population density profile'),
                                               plotOutput('flight_path_population_plot'))),
                               fluidRow(
                                 column(12,
                                        h3('Raw data'),
                                        DT::dataTableOutput('raw_data'))
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
          h3('Project details'),
          p('The NDH is a sub-project of ',
            a(href = 'http://www.stonybrook.edu/commcms/ghi/projects/drots', "Stony Brook University Global Health Institute's Drone Observed Therapy System"),'.')
        ),
        fluidRow(
          h3('Want to contribute?'),
          p('The NDH is an entirely transparent, open-source project. The underlying data and documentation (in the form of an R package) are hosted at ',
            a(href = 'https://github.com/joebrew/nepal', 'on github'),
            ', the application can be downloaded and run locally, and pull requests are more than welcome.')
        ),
        fluidRow(
          h3('App construction and hosting')
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
  
  
  pathx <-reactiveVal(value = 0)
  pathy <-reactiveVal(value = 0)
  distance <- reactiveVal(value = 0)
  
  flight_path <- reactive({
    x <- pathx()
    y <- pathy()
    if(is.null(x) | length(x) <= 1){
      return(NULL)
    } else {
      df <- make_flight_path(x = x,
                                  y = y,
                                  country = input$country)
      return(df)
    }
  })

  # observeEvent(input$flights_marker_click, {
  #   print('MARKER CLICK')
  #   print(input$flights_marker_click)
  # })
  observeEvent(input$flights_draw_new_feature, {
    print('DRAW NEW FEATURE')
    drawn_coordinates <- input$flights_draw_new_feature$geometry$coordinates
    drawn_coordinates <- unlist(drawn_coordinates)
    print(drawn_coordinates)
    if(!is.null(drawn_coordinates)){
      if(length(drawn_coordinates) >= 2){
        x <- drawn_coordinates[seq(1, length(drawn_coordinates), 2)]
        y <- drawn_coordinates[seq(2, length(drawn_coordinates), 2)]
        pathx(x)
        pathy(y)  
      }    
    }
  })
    
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
  
  output$pyuthan_elevation <- renderLeaflet({
    
    r <- pyuthan_elevation_detailed
    # cols <- c("#0C2C84", "#41B6C4", 'brown', "#FFFFCC")
    cols <- rainbow(10)
    pal <- colorNumeric(cols, values(r),
                        na.color = "transparent")
    
    leaflet() %>% addTiles() %>%

      addRasterImage(r, colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = values(r),
                title = "Elevation") 
    
  })
  
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.pdf",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(fp = flight_path(),
                     dir = getwd(),
                     country = input$country)
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )

  output$geo_coordinates_ui <- renderUI({
    fp <- flight_path()
    print(head(fp))
    ok <- FALSE
    if(!is.null(fp)){
      if(nrow(fp) > 1){
        ok <- TRUE 
      }
    }
    if(ok){
      fluidPage(
        fluidRow(
          column(6,
                 h3('Download report'),
                 downloadButton('report', 'Download report.', width = '100%')),
          column(6,
                 h3('Download raw data'),
                 downloadButton("downloadData", "Download raw data", width = '100%')),
          h3('Geo-coordinates'),
          plotOutput('flight_path_plot')
        )
      )
    } else {
      fluidPage(
        fluidRow(
          h3('Geo-coordinates'),
          h5('First create a flight path on the map.')
        )
      ) 
    }
  })
  
  output$flights <- renderLeaflet({
    require(leaflet.extras)
    
    ic <- input$country
    if(is.null(ic)){
      return(NULL)
    }
    if(ic == 'Nepal'){
      r <- pyuthan_elevation_detailed
      district_borders <- nepal::pyuthan
    } else {
      r <- fiana_elevation_small
      district_borders <- nepal::fiana
    }
    
    # cols <- c("#0C2C84", "#41B6C4", 'brown', "#FFFFCC")
    cols <- rainbow(10)
    pal <- colorNumeric(cols, values(r),
                        na.color = "transparent")
    
    mydrawPolylineOptions <- 
      function(allowIntersection = TRUE, 
                drawError = list(color = "#b00b00", timeout = 2500), 
                guidelineDistance = 20, 
                metric = TRUE, 
                feet = FALSE, 
                zIndexOffset = 2000, 
                shapeOptions = drawShapeOptions(fill = FALSE), 
                repeatMode = FALSE) {
        leaflet::filterNULL(list(allowIntersection = allowIntersection, 
                                 drawError = drawError, 
                                 uidelineDistance = guidelineDistance, 
                                 metric = metric, 
                                 feet = feet, 
                                 zIndexOffset = zIndexOffset,
                                 shapeOptions = shapeOptions,  
                                 repeatMode = repeatMode)) }
    
    hf_lab <- hf[hf@data$type == 'Hub',]
    hf_hp <- hf[hf@data$type == 'Health post',]
    hf_shp <- hf[hf@data$type == 'Sub health post',]
    
    hf_lab_icons <- awesomeIcons(
      icon = 'hospital',
      iconColor = 'black',
      library = 'fa',
      markerColor = 'red',
      spin = TRUE
    )
    hf_hp_icons <- awesomeIcons(
      icon = 'medkit',
      iconColor = 'white',
      library = 'fa',
      markerColor = 'orange'
    )
    hf_shp_icons <- awesomeIcons(
      # squareMarker = TRUE,
      icon = 'home',
      iconColor = 'blue',
      library = 'fa',
      markerColor = 'white'
    )
    
    
    leaflet() %>% 
      addProviderTiles(providers$OpenTopoMap) %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = 'Satellite') %>%
      addProviderTiles(providers$OpenStreetMap,
                       group = 'OSM') %>%
      addProviderTiles(providers$Hydda.RoadsAndLabels, group = 'Place names', options = providerTileOptions(zIndex = 1000000)) %>%
      # # add graticules from a NOAA webserver
      # addWMSTiles(
      #   "https://gis.ngdc.noaa.gov/arcgis/services/graticule/MapServer/WMSServer/",
      #   layers = c("1-degree grid", "5-degree grid"),
      #   options = WMSTileOptions(format = "image/png8", transparent = TRUE),
      #   attribution = NULL,group = 'Graticules') %>%
      addRasterImage(r, colors = pal, opacity = 0.6,
                     group = 'Elevation') %>%
      addAwesomeMarkers(data = hf_hp,
                 icon = hf_hp_icons,
                 # popup = hf_hp$VDC_NAME1,
                 label = hf_hp$VDC_NAME1,
                 group = 'Health posts') %>%
      addAwesomeMarkers(data = hf_shp,
                 icon = hf_shp_icons,
                 # popup = hf_hp$VDC_NAME1,
                 label = hf_shp$VDC_NAME1,
                 group = 'Sub health posts') %>%
      addAwesomeMarkers(data = hf_lab,
                 icon = hf_lab_icons,
                 # popup = hf_hp$VDC_NAME1,
                 label = hf_lab$VDC_NAME1,
                 group = 'Hospitals/labs') %>%
      addLegend(pal = pal, values = values(r),
                position = 'bottomleft',
                title = "Elevation") %>%
      # addSearchOSM() %>%
      # addSearchGoogle() %>%
      addDrawToolbar(
        polylineOptions = mydrawPolylineOptions(metric=TRUE, feet=FALSE),
        editOptions=editToolbarOptions(edit = FALSE,
                                       remove = TRUE,
                                       selectedPathOptions=selectedPathOptions()),
        # editOptions = TRU,
        polygonOptions = FALSE,
        circleOptions = FALSE,
        rectangleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE) %>%
        addResetMapButton() %>%
      addLayersControl(overlayGroups = c(
        'Elevation',
        'Hospitals/labs', 'Health posts', 'Sub health posts',
        'Place names',
                                         # 'Graticules',
                                         'Satellite',
                                         'OSM',
                                         'District borders',
                                         'Day/Night'),
                       options = layersControlOptions(collapsed = FALSE),
                       position = 'bottomright') %>%
      addTerminator(group = 'Day/Night') %>%
      hideGroup(c('Place names')) %>%
      hideGroup(c('Graticules')) %>%
      hideGroup(c('Satellite')) %>%
      hideGroup(c('OSM')) %>%
      hideGroup(c('Day/Night')) %>%
      addPolylines(data = district_borders,
                   color = 'red',
                   weight = 2,
                   group = 'District borders') %>%
      addScaleBar(position = 'topright') %>%
      addMiniMap(
        tiles = providers$Esri.WorldStreetMap,
        toggleDisplay = TRUE) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))
    
    
      
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('raw', ".csv", sep = "")
    },
    content = function(file) {
      write.csv(flight_path(), file, row.names = FALSE)
    }
  )
  
  output$raw_data <- DT::renderDataTable({
    df <- flight_path()
    if(!is.null(df)){
      if(nrow(df) > 0){
        df <- df %>%
          dplyr::rename(longitude = x,
                        latitude = y,
                        distance = d)
        df$distance<- round(df$distance)
        databrew::prettify(df,
                           download_options = TRUE,
                           round_digits = 5)
      }
    }
  })
  
  output$flight_path_plot <- renderPlot({
    df <- flight_path()
    if(!is.null(df)){
      if(nrow(df) > 0){
        make_flight_path_plot(df = df)
      }
    }
  })
  
  output$flight_path_elevation_plot <- renderPlot({
    df <- flight_path()
    
    if(!is.null(df)){
      if(nrow(df) > 0){
        make_flight_path_elevation_plot(df = df)
      }
    }
  })
  
  output$flight_path_population_plot <- renderPlot({
    df <- flight_path()
    save(df, file = 'tmp.RData')
    if(!is.null(df)){
      if(nrow(df) > 0){
        make_flight_path_population_plot(df = df)
      }
    }
  })
  
  output$pyuthan_elevation_rough <- renderLeaflet({
    
    r <- pyuthan_elevation
    # cols <- c("#0C2C84", "#41B6C4", 'brown', "#FFFFCC")
    cols <- rainbow(10)
    pal <- colorNumeric(cols, values(r),
                        na.color = "transparent")
    
    leaflet() %>% addTiles() %>%
      addRasterImage(r, colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = values(r),
                title = "Elevation") 
    
  })
  
  output$pyuthan_population <- renderLeaflet({
    
    r <- pyuthan_population
    cols <- c(rainbow(10))#  "#41B6C4", 'brown', "#FFFFCC")
    pal <- colorNumeric(cols, values(r),
                        na.color = "transparent")
    
    leaflet() %>% addTiles() %>%
      addRasterImage(r, colors = pal, opacity = 0.8) %>%
      addLegend(pal = pal, values = values(r),
                title = "Population")
    
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
  
  tv_data <- reactive({
    df <- goog
    # Perform filter operations here
    fg <- input$filter_groups
    ft <- input$filter_types
    fp <- input$filter_people
    if(!is.null(fg)){
      df <- df %>% filter(Group %in% fg)
    }
    if(!is.null(ft)){
      df <- df %>% filter(Type %in% ft)
    }
    if(!is.null(fp)){
      if(fp != ''){
        if(fp != ' '){
          df <- df %>%
            filter(grepl(tolower(fp), tolower(Responsible)))
        }
      }
    }
    # print(head(df))
    df$id <- 1:nrow(df)
    
    df
  })
  
  output$tv_text <- renderText(
    paste(input$tv_selected, collapse = " ")
  )

  
  output$tv_table <- DT::renderDataTable({
    df <- tv_data()
    df
  })
  
  output$tv <- renderTimevis({
    
    df <- tv_data()
    df$type <- ifelse(is.na(df$End), 'point', 'range')
    group_df <- df %>%
      group_by(content = Group) %>%
      summarise(id = 1) %>%
      mutate(id = cumsum(id))
    df$group <- as.numeric(factor(df$Group))
    df <- df %>%
      dplyr::rename(content = Event,
                    start = Start,
                    end = End) %>%
      mutate(content = paste0(
        '<h5>', content,
        '</h5>',
        '<h5>', format(start, '%b %d'),
        ifelse(!is.na(end), paste0(' to ', format(end, '%b %d'), collapse = NULL), ''),
        '</h5>',
        
      # '</br>',
      # '</p>',
      # '<p>',
      # '<br>',
      
      '<h6>',
      ifelse(!is.na(Details),
             paste0('(',Details,
                    ifelse(!is.na(Responsible),
                           paste0('[',
                                  Responsible,
                                  ']',
                                  collapse = NULL),
                           ''),
                    ')', collapse = NULL),
             ifelse(!is.na(Responsible),
                    paste0('[',
                           Responsible,
                           ']',
                           colapse = NULL),
                    '')),
      '</h6>'
      ))
    
    config <- list(
      # editable = TRUE,
      multiselect = TRUE)
      
    timevis(data = df,
            groups = group_df,
            options = config)
    
  })

}

shinyApp(ui, server)