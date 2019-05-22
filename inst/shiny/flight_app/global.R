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
unique_flights <- waypoints %>%
  group_by(take_off, landing) %>% tally %>%
  dplyr::select(-n)
exact_coords <- waypoints %>%
  filter(!duplicated(take_off)) 
exact_coords <- exact_coords %>%
  dplyr::select(take_off, longitude, latitude) %>%
  dplyr::rename(site_name = take_off)
