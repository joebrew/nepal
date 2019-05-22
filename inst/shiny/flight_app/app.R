library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(nepal)
library(owmr)
# Define site names
site_names <- c('',
                sort(unique(waypoints$take_off)))
# Define safety choices
safety_choices_a <- c('Batteries charged',
                    'Landing gear in place',
                    'Rotors show no visible damage',
                    'Container securely closed and fastened')
safety_choices_b <- c('Take-off site clear',
                      'Take-off site weather good',
                      'Landing site clear',
                      'Landing site weather good',
                      'Relay stations OK')

# Function for checking log-in
check_password <- function(user, password){
  message('User/password combination is correct')
  if(user == 'joe'){
    return(TRUE)
  } else {
    return(FALSE)
  }
  # Replace code above with password validation
}



# Function for adding new user
add_user <- function(user, password){
  message('Account just created with the following credentials')
  message('---User: ', user)
  message('---Password: ', password)
  # Add code here to add user to database
}

header <- dashboardHeader(title="DrOTS Air Operations")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text="Main",
      tabName="main",
      icon=icon("eye")),
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
          h1('Air operations')
        ),
        fluidRow(
          column(2,
                 selectInput('take_off',
                             'Taking off from',
                             choices = site_names)),
          column(2,
                 uiOutput('ui_landing')),
          column(3,
                 uiOutput('ui_a')),
          column(3,
                 uiOutput('ui_b')),
          column(2,
                 uiOutput('ui_c'))
        ),
        fluidRow(
          column(6,
                 uiOutput('ui_maps')),
          column(6,
                 uiOutput('ui_weather'))
        )
      )
    ),
    tabItem(
      tabName = 'about',
      fluidPage(
        fluidRow(
          div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
          h4('Hosted by ',
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
  
  # Reactive values
  logged_in <- reactiveVal(value = FALSE)
  modal_text <- reactiveVal(value = '')
  # Log in modal
  showModal(
    modalDialog(
      uiOutput('modal_ui'),
      footer = NULL
    )
  )
  
  
  # See if log-in worked
  observeEvent(input$submit, {
    cp <- check_password(user = input$user,
                         password = input$password)
    logged_in(cp)
  })
  
  # When OK button is pressed, attempt to log-in. If success,
  # remove modal.
  observeEvent(input$submit, {
    # Did login work?
    li <- logged_in()
    if(li){
      # Update the reactive modal_text
      modal_text(paste0('Logged in as ', input$user))
      removeModal()
    } else {
      # Update the reactive modal_text
      modal_text(paste0('That user/password combination is not correct.'))
    }
  })
  
  # Make a switcher between the log in vs. create account menus
  create_account <- reactiveVal(FALSE)
  observeEvent(input$create_account,{
    currently <- create_account()
    nowly <- !currently
    create_account(nowly)
  })
  observeEvent(input$submit_create_account,{
    currently <- create_account()
    nowly <- !currently
    create_account(nowly)
  })
  observeEvent(input$back,{
    currently <- create_account()
    nowly <- !currently
    create_account(nowly)
  })
  
  output$modal_ui <- renderUI({
    # Capture the modal text.
    mt <- modal_text()
    # See if we're in account creation vs log in mode
    account_creation <- create_account()
    if(account_creation){
      fluidPage(
        fluidRow(
          column(12,
                 align = 'right',
                 actionButton('back',
                              'Back'))
        ),
        h3(textInput('create_user', 'Create username'),
           textInput('create_password', 'Create password')),
        fluidRow(
          column(12, align = 'right',
                 actionButton('submit_create_account',
                              'Create account'))
        )
      )
    } else {
      fluidPage(
        h3(textInput('user', 'Username',
                     value = 'joe'),
           passwordInput('password', 'Password')),
        fluidRow(
          column(6,
                 actionButton('submit',
                              'Submit')),
          column(6, align = 'right',
                 actionButton('create_account',
                              'Create account'))
        ),
        p(mt)
      )}
  })
  
  # Observe submission of flight data
  observeEvent(input$submit_flight,{
    checklist_a <- input$checklist_a
    checklist_b <- input$checklist_b
    ok <- TRUE
    if(length(checklist_a) != length(safety_choices_a) |
       length(checklist_b) != length(safety_choices_b)){
      ok <- FALSE
    }
    
    confirm_text <- 
      paste0(input$take_off, ' to ',
             input$landing)
    
    if(!ok){
      showModal(
        modalDialog(
          fluidPage(
            h3('Are you sure?'),
            p(paste0(
              'You are about to fly from ',
              confirm_text,
              ', but you have not checked all the items on the safety checklists. If you are sure you are ready to deploy this flight, please write your reason for not checking an item(s) below. Otherwise, click outside of this box and finish the safety checklists.'
            )),
            textInput('confirm_comments',
                      ''),
            actionButton('confirm',
                         'Confirm')
          ),
          footer = NULL,
          easyClose = TRUE
        )
      )
    } else {
      showModal(
        modalDialog(
          h3('Are you sure?'),
          p(paste0('Please confirm that you are about to fly from ', confirm_text, '. If you have any comments on this flight, please write them below (not required).')),
          textInput('confirm_comments',
                    ''),
          actionButton('confirm',
                       'Confirm'),
          footer = NULL,
          easyClose = TRUE
        )
      )
    }
  })
  
  # Observe account creation
  observeEvent(input$submit_create_account,{
    add_user(user = input$create_user,
             password = input$create_password)
  })
  
  # Ui for flight planning
  output$ui_a <- renderUI({
    take_off <- input$take_off
    landing <- input$landing
    ok <- FALSE
    if(!is.null(take_off) &
       !is.null(landing)){
      if(take_off != '' &
         landing != ''){
        ok <- TRUE
      }
    }
    if(!ok){
      NULL
    } else {
      checkboxGroupInput('checklist_a',
                         'Drone safety checklist',
                         choices = safety_choices_a)
    }
  })
  output$ui_b <- renderUI({
    take_off <- input$take_off
    landing <- input$landing
    ok <- FALSE
    if(!is.null(take_off) &
       !is.null(landing)){
      if(take_off != '' &
         landing != ''){
        ok <- TRUE
      }
    }
    if(!ok){
      NULL
    } else {
      checkboxGroupInput('checklist_b',
                         'Conditions safety checklist',
                         choices = safety_choices_b)
    }
  })
  output$ui_c <- renderUI({
    take_off <- input$take_off
    landing <- input$landing
    ok <- FALSE
    if(!is.null(take_off) &
       !is.null(landing)){
      if(take_off != '' &
         landing != ''){
        ok <- TRUE
      }
    }
    if(!ok){
      NULL
    } else {
      actionButton('submit_flight',
                   'Submit flight data')
    }
  })
  
  # Ui for maps
  output$ui_maps <- renderUI({
    take_off <- input$take_off
    landing <- input$landing
    ok <- FALSE
    if(!is.null(take_off) &
       !is.null(landing)){
      if(take_off != '' &
         landing != ''){
        ok <- TRUE
      }
    }
    if(!ok){
      NULL
    } else {
     fluidPage(h1('Route'),
               leafletOutput('leafy'))

    }
  })
  
  # Weather ui
  weather_table <- reactive({
    ok <- FALSE
    take_off_site <- input$take_off
    landing_site <- input$landing
    if(!is.null(take_off_site) &
       !is.null(landing_site)){
      if(take_off_site != '' &
         landing_site != ''){
       ok <- TRUE 
      }
    }
    if(ok){
      take_off_coords <- exact_coords %>%
        filter(site_name == take_off_site)
      landing_coords <- exact_coords %>%
        filter(site_name == landing_site)

      
      take_off_weather <- get_current(lon = take_off_coords$longitude,
                                      lat = take_off_coords$latitude)
      landing_weather <- get_current(lon = landing_coords$longitude,
                                      lat = landing_coords$latitude)
      if(length(take_off_weather) >2 &
         length(landing_weather) > 2){
        take_off_weather <- take_off_weather %>%
          owmr_as_tibble()
        landing_weather <- landing_weather %>%
          owmr_as_tibble()
        convert_weather <- function(x){
          y <- x %>% dplyr::select(temp,
                              humidity,
                              weather_description,
                              wind_speed,
                              wind_deg,
                              dt_sunrise_txt,
                              dt_sunset_txt) %>%
            mutate(temp = temp / 10) %>%
            dplyr::rename(description = weather_description,
                          sunrsise = dt_sunrise_txt,
                          sunset = dt_sunset_txt,
                          wind_degrees = wind_deg,
                          temperature = temp)
          names(y) <- Hmisc::capitalize(gsub('_', ' ', names(y)))
          return(y)
        }
        weather <- bind_rows(
          take_off_weather %>% convert_weather() %>% mutate(Where = 'Take-off'),
          landing_weather %>% convert_weather() %>% mutate(Where = 'Landing')
        ) %>%
          gather(key, value, Temperature:Sunset) %>%
          arrange(Where)
        weather
      }
    }
  })
  
  output$take_off_table <- renderTable({
    this_table <- weather_table()
    if(!is.null(this_table)){
      if(nrow(this_table) > 0){
        out <- this_table %>% filter(Where == 'Take-off')
        out %>% dplyr::select(-Where)
      }
    }
  })
  output$landing_table <- renderTable({
    this_table <- weather_table()
    if(!is.null(this_table)){
      if(nrow(this_table) > 0){
        out <- this_table %>% filter(Where == 'Landing')
        out %>% dplyr::select(-Where)
      }
    }
  })
  
  output$ui_weather <- renderUI({
    this_table <- weather_table()
    if(!is.null(this_table)){
      if(nrow(this_table) > 0){
        print(this_table)
        save(this_table, file = 'coords.RData')
        take_off_table <- this_table %>% filter(Where == 'Take-off')
        landing_table <- this_table %>% filter(Where == 'Landing')
        fluidPage(
          h1('Weather'),
          h3('Weather at take-off site'),
          tableOutput('take_off_table'),
          h3('Weather at landing site'),
          tableOutput('landing_table'))
      }
    }
  })
  
  output$ui_landing <- renderUI({
    take_off_site <- input$take_off
    possible_landing_sites <- unique_flights %>%
      filter(take_off == take_off_site) 
    possible_landing_sites <- sort(unique(possible_landing_sites$landing))
    if(length(possible_landing_sites) > 1){
      possible_landing_sites <- c('',
                                  possible_landing_sites)
    }
    
    selectInput('landing',
                'Landing at',
                choices = possible_landing_sites)
  })
  
  output$leafy <- renderLeaflet({
    take_off_site <- input$take_off
    landing_site <- input$landing
    sub_way <- waypoints %>%
      filter(take_off == take_off_site,
             landing == landing_site)
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addPolylines(data = sub_way,
                   lng = ~longitude,
                   lat = ~latitude) %>%
      addMarkers(data = sub_way[1,],
                 lng = ~longitude,
                 lat = ~latitude,
                 popup = 'Take-off') %>%
      addMarkers(data = sub_way[nrow(sub_way),],
                 lng = ~longitude,
                 lat = ~latitude,
                 popup = 'Landing')
  })
  
}

shinyApp(ui, server)