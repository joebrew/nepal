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
    user_email = c('joebrew@gmail.com', 'joe@databrew.cc'),
    created_at = Sys.time(),
    take_off = c('Binghri', 'Pyuthan'),
    landing = c('Siri', 'Dharmawati'),
    checklist_complete = c(TRUE, TRUE),
    status = c('Success', 'Success'),
    stringsAsFactors = FALSE)

# Add tables to database
write_table(connection_object = co,
            table = 'users',
            value = users)
write_table(connection_object = co,
            table = 'flights',
            value = flights)
