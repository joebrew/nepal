local <- TRUE
library(nepal)
library(tidyverse)
library(yaml)
library(owmr)

# Get credentials
if(local){
  creds_file <- 'credentials/credentials_local.yaml'
} else {
  creds_file <- 'credentials/credentials.yaml'
}
creds <- yaml::yaml.load_file(creds_file)

# Read in open weather map key
owm_key <- creds$owm_key
# owmr_settings(owm_key)
Sys.setenv(OWM_API_KEY = owm_key)

# Read in the waypoints
waypoints <- nepal::plans
waypoints <- waypoints %>% filter(longitude != 0)
unique_flights <- waypoints %>%
  group_by(take_off, landing) %>% tally %>%
  dplyr::select(-n)
exact_coords <- waypoints %>%
  filter(!duplicated(take_off)) 
exact_coords <- exact_coords %>%
  dplyr::select(take_off, longitude, latitude) %>%
  dplyr::rename(site_name = take_off)

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
check_password <- function(user, password, users){
  if(user %in% users$user_email){
    user_data <- users %>% filter(user_email == user)
    if(password == user_data$user_password){
      message('Password ok')
      return(TRUE)
    }
    else
      message('User exists, but password is wrong')
    return(FALSE)
  } else {
    return(FALSE)
  }
}


# Function for adding new user
add_user <- function(user, password, connection_object, users){
  message('---User: ', user)
  message('---Password: ', password)
  new_user_data <- tibble(user_email = user,
                          user_password = password,
                          created_at = Sys.time())
  if(user %in% users$user_email){
    message('Account could not be created because ',
            user, ' already exists.')
    out <- FALSE
  } else {
    message('Account created with the following credentials')
    out <- TRUE
    # Add user to database
    update_db(data = new_user_data,
              table_name = 'users',
              connection_object = connection_object)
  }
  return(out)
}

# Connect to database
# Connect to database
if(!local){
  # REMOTE
  credentials <- credentials_extract(credentials_file = 'credentials/credentials.yaml', all_in_file = TRUE)
} else {
  # LOCAL
  credentials <- credentials_extract(credentials_file = 'credentials/credentials_local.yaml', all_in_file = TRUE)
}

co <- credentials_connect(options_list = credentials)


# Read from database
flights <- get_data(query = 'SELECT * FROM flights',
                    connection_object = co)
users <- get_data(query = 'SELECT * FROM users',
                    connection_object = co)
tb <- get_data(query = 'SELECT * FROM tb',
                  connection_object = co)