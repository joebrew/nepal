# Define whether setting up local or remote db
local <- TRUE

# To be run just once
# First, create database in psql:
# CREATE DATABASE flights

# Load up libraries
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tidyr)
library(gsheet)
library(yaml)
library(httr)
library(RPostgreSQL)

# Define health_post locations


# Connect to database
if(!local){
  # REMOTE
  credentials <- credentials_extract(credentials_file = 'credentials/credentials.yaml', all_in_file = TRUE)
} else {
  # LOCAL
  credentials <- credentials_extract(credentials_file = 'credentials/credentials_local.yaml', all_in_file = TRUE)
}

co <- credentials_connect(options_list = credentials)

# Create tables
users <- data.frame(
  user_email = c('joebrew@gmail.com', 'joe@databrew.cc'),
  user_password = 'password',
  created_at = Sys.time(),
  stringsAsFactors = FALSE)

flights <- data.frame(
  flight_number = 1:2,
  user_email = c('joebrew@gmail.com', 'joe@databrew.cc'),
  created_at = Sys.time(),
  take_off = c('Bhingri', 'Pyuthan'),
  landing = c('Saari', 'Dharmawati'),
  checklist_missing = c('',''),
  status = c('Success', 'Success'),
  cargo_quantity = c(0,3),
  cargo_item = c('Nothing', 'Sputum samples'),
  comment = '',
  stringsAsFactors = FALSE)

# Add tables to database
write_table(connection_object = co,
            table = 'users',
            value = users)
write_table(connection_object = co,
            table = 'flights',
            value = flights)

# Disconnect from the db
RPostgreSQL::dbDisconnect(co)
