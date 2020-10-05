# MarineGEO data submission app example 
# This is the global script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu

# Load packages
library(shiny)
library(shinyjs)
library(readxl)
library(tidyverse)
library(rdrop2)
library(lubridate)
library(magrittr)
library(rmarkdown)
library(markdown)
library(knitr)
library(DT)
library(anytime)

# Portal version
portal_version <- "v0.5.0"

# Testing modes: 
# IF ANY VALUES ARE TRUE, THEN TESTING IS ACTIVE
# No data transmission
# FALSE = Objects saved to DB
# TRUE = Objects not saved to DB
no_db_testing <- F

# Dropbox testing
# FALSE = Objects saved to official directory
# TRUE = Objects saved to test directory
dropbox_testing <- F

if(dropbox_testing){
  destination <- "marinegeo_resources/test_data_destination/"
} else {
  destination <- "MarineGEO/Data/"
}

# function to stamp files with the time of submission
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

submissionDate <- function() format(Sys.time(), "%Y%m%d")
  
# footer disclaimer code
footer <- tags$footer(tags$a(href="https://www.si.edu/Privacy", "View our privacy statement"),
                      " Beta version: Send feedback to MarineGEO@si.edu", 
                      align = "center", 
                      style = "
                                          position:fixed;
                                          bottom:0;
                                          width:100%;
                                          height:20px; 
                                          color: black;
                                          background-color: yellow;
                                          z-index: 1000;"
)



# Authenticate your dropbox token 
drop_auth(rdstoken = "./droptoken.rds")

# Read in protocol structure table
protocol_structure <- read_csv("./data/protocol_structure.csv")

# Data type and availability table for the data policy 
data_policy_table <- read_csv("./data/data_policy_table.csv")

# Record working directory to return to after moving to temporary directory
original_wd <- getwd()

# roster <- drop_read_csv("MarineGEO/Data/resources/marinegeo_roster.csv", encoding = "UTF-8") %>%
#   mutate(email = as.character(email)) %>%
#   mutate(email = tolower(email))

warnings <- read_csv("./data/warnings_lookup.csv")


