library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(nepal)
library(owmr)

# Source set up file
source('global.R')

header <- dashboardHeader(title="DrOTS Dashboard",
                          tags$li(class = 'dropdown',  
                                  tags$style(type='text/css', "#log_out_ui {margin-right: 10px; margin-left: 10px; font-size:80%; margin-top: 10px; margin-bottom: -10px;}")),
                                  tags$li(class = 'dropdown',
                                          uiOutput('log_out_ui')))
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text="Flight planner",
      tabName="main",
      icon=icon("eye")),
    menuItem(
      text="Flight status updater",
      tabName="history",
      icon=icon("eye")),
    menuItem(
      text = 'Ground operations',
      tabName = 'ground',
      icon = icon('eye')),
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
      tabName = 'history',
      uiOutput('ui_history')
    ),
    tabItem(
      tabName = 'ground',
      h1('Ground operations data entry'),
      uiOutput('ui_ground')
    ),
    tabItem(
      tabName="main",
      fluidPage(
        fluidRow(
          h1('Air operations')
        ),
        fluidRow(
          column(4,
                 fluidRow(
                   column(6,
                          selectInput('take_off',
                                      'Taking off from',
                                      choices = site_names)),
                   column(6,
                          uiOutput('ui_landing'))
                 ),
                 fluidRow(uiOutput('ui_cargo')),
                 fluidRow(uiOutput('ui_flight_number'))),
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
                 uiOutput('ui_photo'))
        ),
        fluidRow(
          column(12,
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
server <- function(input, output, session) {
  
  # Reactive values
  data <- reactiveValues(flights = flights,
                         users = users,
                         tb = tb)
  logged_in <- reactiveVal(value = FALSE)
  user <- reactiveVal(value = '')
  modal_text <- reactiveVal(value = '')
  creation_text <- reactiveVal(value = '')
  flight_number <- reactiveVal(value = nrow(flights) + 1)
  # Log in modal
  showModal(
    modalDialog(
      uiOutput('modal_ui'),
      footer = NULL
    )
  )
  
  # Observe log-out and show modal
  observeEvent(input$log_out,{
    # Update the reactive values
    modal_text('')
    logged_in(FALSE)
    user('')
    creation_text('')
    # Log in modal
    showModal(
      modalDialog(
        uiOutput('modal_ui'),
        footer = NULL
      )
    )
  })
  
  
  # See if log-in worked
  observeEvent(input$submit, {
    cp <- check_password(user = input$user,
                         password = input$password,
                         users = data$users)
    logged_in(cp)
    if(cp){
      user(input$user)
    }
  })
  
  # Observe the log out button
  observeEvent(input$log_out, {
    logged_in(FALSE)
    user('')
  })
  
  output$log_out_ui <- renderUI({
    li <- logged_in()
    if(li){
      tags$li(class = 'dropdown',
              actionButton('log_out', label = 'Log out', icon = icon('times')))
      
    } else {
      NULL
    }
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
  account_creation_switcher <- reactiveVal(FALSE)
  observeEvent(input$create_account,{
    currently <- account_creation_switcher()
    nowly <- !currently
    account_creation_switcher(nowly)
  })
  # Observe account creation
  observeEvent(input$submit_create_account,{
    user_added <- 
      add_user(user = input$create_user,
               users = data$users,
               password = input$create_password,
               connection_object = co)
    message('user_added is ', user_added)
    if(user_added){
      # update the reactive values
      data$users <- get_data(tab = 'users',
                             dbname = 'flights',
                             connection_object = co)
      creation_text('')
    } else {
      creation_text('An account already exists for that user.')
    }
  })
  
  observeEvent(input$submit_create_account,{
    currently <- account_creation_switcher()
    ct <- creation_text()
    if(ct == ''){
      nowly <- !currently
      account_creation_switcher(nowly)
    }
    
  })
  observeEvent(input$back,{
    currently <- account_creation_switcher()
    nowly <- !currently
    account_creation_switcher(nowly)
  })
  
  output$modal_ui <- renderUI({
    # Capture the modal text.
    mt <- modal_text()
    # Capture the account creation text
    ct <- creation_text()
    # See if we're in account creation vs log in mode
    account_creation <- account_creation_switcher()
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
        ),
        fluidRow(
          column(12,
                 ct)
        )
      )
    } else {
      fluidPage(
        h3(textInput('user', 'Username',
                     value = 'joe@databrew.cc'),
           passwordInput('password', 'Password',
                         value = 'password')),
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
  
  missing_checklist <- reactive({
    safety_choices <- c(safety_choices_a,
                        safety_choices_b)
    complete <- c(input$checklist_a,
                  input$checklist_b)
    missing <- safety_choices[!safety_choices %in% complete]
    paste0(missing, collapse = '; ')
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
             input$landing, ' with ',
             ifelse(input$cargo_quantity == 0,
                    'no cargo',
                    paste0(input$cargo_quantity,
                           ' ',
                           input$cargo_item, 
                           collapse = '')))
    
    # Flight number
    fn <- flight_number()
    
    if(!ok){
      showModal(
        modalDialog(
          fluidPage(
            h3('Are you sure?'),
            p(paste0(
              'You are about to fly from ',
              confirm_text,
              '. This will be flight number ', fn
            ),
            p('However, you have not checked all the items on the safety checklists. If you are sure you are ready to deploy this flight, please write your reason for not checking an item(s) below. Otherwise, click outside of this box and finish the safety checklists.')),
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
          p(paste0('Please confirm that you are about to fly from ', confirm_text, '. This will be flight number ', fn, 
                   ' If you have any comments on this flight, please write them below (not required).')),
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
  
  # Observe the flight confirmation and show an output modal
  observeEvent(input$confirm,{
    showModal(
      modalDialog(
        uiOutput('confirmed_ui'),
        footer = NULL,
        easyClose = TRUE
      )
    )
    # Update the database of flights
    flight_info <- tibble(
      flight_number = flight_number(),
      user_email = user(),
      created_at = Sys.time(),
      take_off = input$take_off,
      landing = input$landing,
      checklist_missing = missing_checklist(),
      status = '',
      cargo_quantity = input$cargo_quantity,
      cargo_item = input$cargo_item,
      comment = input$confirm_comments
    )
    update_db(data = flight_info,
              table_name = 'flights',
              connection_object = co)
    # Update the reactive objects
    data$flights <- get_data(query = 'SELECT * FROM flights',
                             connection_object = co)
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
    # data.frame()
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
  
  forecast_table <- reactive({
    # data.frame()
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


      take_off_weather <- get_forecast(lon = take_off_coords$longitude,
                                      lat = take_off_coords$latitude)
      landing_weather <- get_forecast(lon = landing_coords$longitude,
                                     lat = landing_coords$latitude)
      if(length(take_off_weather) >2 &
         length(landing_weather) > 2){
        take_off_weather <- take_off_weather %>%
          owmr_as_tibble()
        landing_weather <- landing_weather %>%
          owmr_as_tibble()
        add_rain <- function(x){
          if('rain_3h' %in% names(x)){
            x$rain_3h <- NA
            return(x)
          }
        }
        take_off_weather <- add_rain(take_off_weather)
        landing_weather <- add_rain(landing_weather)
        save(take_off_weather,
             landing_weather,
             file = 'weather.RData')
        convert_forecast <- function(x){
          y <- x %>% dplyr::select(dt_txt,
                                   temp,
                                   humidity,
                                   weather_description,
                                   wind_speed,
                                   wind_deg,
                                   clouds_all,
                                   rain_3h) %>%
            mutate(temp = temp / 10) %>%
            dplyr::rename(description = weather_description,
                          cloud_cover = clouds_all,
                          date_time = dt_txt,
                          rain = rain_3h,
                          wind_degrees = wind_deg,
                          temperature = temp)
          names(y) <- Hmisc::capitalize(gsub('_', ' ', names(y)))
          return(y)
        }
        weather <- bind_rows(
          take_off_weather %>% convert_forecast() %>% mutate(Where = 'Take-off'),
          landing_weather %>% convert_forecast() %>% mutate(Where = 'Landing')
        ) %>%
          # gather(key, value, Temperature:Rain) %>%
          arrange(Where)
        weather
      }
    } else {
      NULL
    }
  })
  
  
  output$take_off_weather_table <- renderTable({
    this_table <- weather_table()
    if(!is.null(this_table)){
      if(nrow(this_table) > 0){
        out <- this_table %>% filter(Where == 'Take-off')
        out %>% dplyr::select(-Where)
      }
    }
  })
  output$landing_weather_table <- renderTable({
    this_table <- weather_table()
    if(!is.null(this_table)){
      if(nrow(this_table) > 0){
        out <- this_table %>% filter(Where == 'Landing')
        out %>% dplyr::select(-Where)
      }
    }
  })
  
  output$take_off_forecast_table <- renderTable({
    this_table <- forecast_table()
    if(!is.null(this_table)){
      if(nrow(this_table) > 0){
        out <- this_table %>% filter(Where == 'Take-off')
        out <- out[1:3,]
        out %>% dplyr::select(-Where)
      }
    }
  })
  output$landing_forecast_table <- renderTable({
    this_table <- forecast_table()
    if(!is.null(this_table)){
      if(nrow(this_table) > 0){
        out <- this_table %>% filter(Where == 'Landing')
        out <- out[1:3,]
        out %>% dplyr::select(-Where)
      }
    }
  })
  
  
  output$ui_weather <- renderUI({
    this_table <- weather_table()
    if(!is.null(this_table)){
      if(nrow(this_table) > 0){
        take_off_table <- this_table %>% filter(Where == 'Take-off')
        landing_table <- this_table %>% filter(Where == 'Landing')
        fluidPage(
          h1('Weather'),
          tabsetPanel(
            tabPanel(title = 'Current weather',
                     fluidPage(
                       column(6,
                              h3('Weather at take-off site'),
                              tableOutput('take_off_weather_table')),
                       column(6,
                              h3('Weather at landing site'),
                              tableOutput('landing_weather_table')))),
            tabPanel(title = 'Forecast',
                     fluidPage(
                       column(6,
                              h3('Forecast at take-off site'),
                              tableOutput('take_off_forecast_table')),
                       column(6,
                              h3('Forecast at landing site'),
                              tableOutput('landing_forecast_table'))))
          )
        )
      }
    }
  })
  
  output$ui_cargo <- renderUI({
    take_off <- input$take_off
    landing <- input$landing
    ok <- FALSE
    if(!is.null(take_off) & !is.null(landing)){
      if(take_off != '' & landing != ''){
        ok <- TRUE
      }
    }
    if(!ok){
      NULL
    } else {
      fluidRow(
        column(6,
               sliderInput('cargo_quantity',
                           'Quantity',
                           min = 0,
                           max = 50,
                           value = 0,
                           step = 1)),
        column(6,
               selectizeInput('cargo_item',
                              'Item',
                              choices = c('No cargo', 'Sputum samples')))
      )
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
  
  output$ui_ground <- renderUI({
    li <- logged_in()
    
    if(li){
      fluidPage(
        fluidRow(
          column(4, 
                 textInput('ground_id',
                           'ID number')),
          column(4,
                 textInput('ground_name_index_case',
                           'Name of index case')),
          column(4,
                 radioButtons('ground_sex_index_case',
                              'Sex',
                              choices = c('M', 'F', 'Unknown'),
                              selected = 'Unknown'))
        ),
        fluidRow(
          column(4,
                 textInput('ground_name_contact',
                           'Name of contact')),
          column(4,
                 sliderInput('ground_age',
                             'Age',
                             min = 0,
                             max = 100,
                             value = 20, step = 1)),
          column(4,
                 textInput('ground_address',
                           'Address'))
        ),
        fluidRow(
          column(4,
                 textInput('ground_diagnosis_center',
                           'Diagnosis center')),
          column(4,
                 dateInput('ground_test_date',
                           'Test date',
                           min = '2019-01-01',
                           max = '2019-12-31')),
          column(4,
                 textInput('ground_lab_no',
                           'Lab number'))
        ),
        fluidRow(
          column(4,
                 textInput('ground_contact_tracer',
                           'Contact tracer')),
          column(4,
                 textInput('ground_test_result',
                           'Test result')),
          column(4,
                 radioButtons('ground_screening_opd_contact_tracing',
                           'OPD screening or contact tracing',
                           choices = c('OPD screening',
                                       'Contact tracing')))
        ),
        fluidRow(
          column(4,
                 textInput('ground_remarks',
                           'Remarks')),
          column(4,
                 textInput('ground_flight_number',
                           'Flight number')),
          column(4,
                 actionButton('ground_submit',
                              'Submit'))
        )
      )
    } else {
      h3('Please log in first.')
    }
  })
  
  # Observe the submission of ground data and add to database
  observeEvent(input$ground_submit, {
    
    ground <- tibble(
      id = input$ground_id,
      name_index_case = input$ground_name_index_case,
      name_contact = input$ground_name_contact,
      age = input$ground_age,
      address = input$ground_address,
      sex_index_case = input$ground_sex_index_case,
      test_date = input$ground_test_date,
      lab_no = input$ground_lab_no,
      contact_tracer = input$ground_contact_tracer,
      test_result = input$ground_test_result,
      screening_opd_contact_tracing = input$ground_screening_opd_contact_tracing,
      remarks = input$ground_remarks,
      flight_number = input$ground_flight_number)
    
    
    # Update the database
    message('Updating the tb table with the following data')
    # print(ground)
    update_db(data = ground,
              table_name = 'tb',
              connection_object = co)
    # Update the in-session data (reactive)
    # Update the reactive objects
    data$tb <- get_data(query = 'SELECT * FROM tb',
                             connection_object = co)
                   
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
  
  # Post confirmation modal
  output$confirmed_ui <- renderUI({
    fn <- flight_number()
    fluidPage(
      h3('Ready to launch'),
      p('You are ready to launch flight number ', fn, '.'),
      p('After take-off, please monitor the flight and communicate with the health post in-charge'),
      p('When the drone lands, make sure to update the "status" of flight number ', fn, ' in the "Flight status updater" tab.'),
      actionButton('ready', 'Ok.'))
    })
  
  # When the ready to launch button is clicked, hide modal and increase number
  observeEvent(input$ready,{
    removeModal()
    fn <- flight_number()
    flight_number(fn + 1)
    updateSelectInput(session = session,
                      inputId = 'take_off',
                      selected = '')
  })
  
  
  output$image <- renderImage({
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
    if(!ok){
      outfile <- 'images/blank.png'
    } else{
      sites <- c(landing_site, take_off_site)
      sites <- sites[!sites %in% c('Bhingri', 'Pyuthan')]
      outfile <- paste0('images/', sites, '1.png')
      message('outfile is ', outfile)
      outfile <- 'images/any.png'
    }
    
    
    
    list(src = outfile,
                  alt = "")
  },
  deleteFile = FALSE)
  output$ui_photo <- renderUI({
    fluidPage(
      h1('Images'),
      imageOutput('image')
    )
  })
  
  
  
  output$ui_flight_number <- renderUI({
    li <- logged_in()
    if(li){
      fn <- flight_number()
      fluidPage(
        h4('Tentative flight number:'),
        h1(fn),
        helpText('This flight number will not be confirmed until you submit all flight details')
      )
    } else {
      NULL
    }
  })
  
  # Which previous flight is selected
  history_selected <- reactiveVal(value = 0)
  observeEvent(input$history_select,{
    history_selected(input$history_select)
  })
  
  output$ui_history <- renderUI({
    previous_flights <- data$flights
    message('A.')
    print(previous_flights)
    # only keep those with no status
    previous_flights <- previous_flights %>%
      filter(is.na(status) | status == '')
    if(nrow(previous_flights) == 0){
      fluidPage(h3('No previous flights need updating.'))
    } else {
      choices <- previous_flights$flight_number
      names(choices) <- paste0(previous_flights$take_off, ' to ',
                               previous_flights$landing, ' on ',
                               as.character(as.Date(previous_flights$created_at)), ' at ', 
                               substr(as.character(previous_flights$created_at), 12, 20))
      
      ok <- FALSE
      current_status <- ''
      selected_flight <- input$history_select
      if(!is.null(selected_flight)){
        if(is.numeric(selected_flight)){
          ok <- TRUE
        }
      }
      if(ok){
        cs <- previous_flights %>%
          filter(flight_number == selected_flight) 
        if(nrow(cs) == 1){
          current_status <- cs$status
        }
      }
      
      message('history flight choices are')
      print(choices)
      
      fluidPage(
        fluidRow(
          column(12,
                 selectInput('history_select',
                             'Select flight',
                             choices = choices,
                             selected = history_selected()
                 )),
          selectInput('history_status',
                      'Status',
                      choices = c('','Success',
                                  'Failed to take-off',
                                  'Safe bail-out',
                                  'Crash'),
                      selected = current_status),
          textInput('history_comments',
                    'Comments (optional)'),
          actionButton('history_submit',
                       'Submit data'))
        
      )
    }
    
  })
  
  # Observe the history update submission
  observeEvent(input$history_submit, {
    selected_flight <- input$history_select
    status <- input$history_status
    comments <- input$history_comments
    RPostgreSQL::dbSendQuery(conn = co,
                             statement = 
                               paste0("UPDATE flights SET status = '",
                                      status, "' WHERE flight_number = ",
                                      selected_flight, ";"))
    
    RPostgreSQL::dbSendQuery(conn = co,
                             statement = 
                               paste0("UPDATE flights SET comment = '",
                                      comments, "' WHERE flight_number = ",
                                      selected_flight, ";"))
    
    # Update the reactive object
    data$flights <- get_data(query = 'SELECT * FROM flights',
                             connection_object = co)
    
  })
  
  # session$onSessionEnded(function() {
  #   message('Session ended. Closing the connection pool.')
  #   tryCatch(RPostgreSQL::dbDisconnect(co), error = function(e) {message('')
  #     
  #   })
  #   
  # })
}



shinyApp(ui, server)