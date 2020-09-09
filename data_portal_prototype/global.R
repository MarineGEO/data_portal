# MarineGEO data submission app example 
# This is the global script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu

# The Shiny captcha package currently disables all action buttons until it is validated
# Therefore it is put in a uiOutput block and only rendered once the user gets to the right page
# However, once a user reaches that page, they will need to validate it to continue using the application

# This version of the application has also removed the sensitive information prompt and asks for only 1 email, 
# In testing mode, it does not skip the data policy agreement and requires an email and excel file

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
library(httr) # for captcha
library(jsonlite) # for captcha

# Portal version
portal_version <- "v0.4.0"

# Testing mode: 
# FALSE = Objects saved to DB
# TRUE = Objects not saved to DB
testing <- T

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

# roster <- drop_read_csv("MarineGEO/Data/resources/marinegeo_roster.csv")

warnings <- read_csv("./data/warnings_lookup.csv")

recaptcha_credentials <- drop_read_csv("marinegeo_resources/credentials/recaptcha_credentials.csv")

site_key <- as.character(recaptcha_credentials[1,1])
secret_key <- as.character(first(recaptcha_credentials[1,2]))

