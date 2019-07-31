# MarineGEO data submission app example 
# This is the global script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu

# Load packages
library(shiny)
library(shinyjs)
library(readxl)
library(tidyverse)
library(rdrop2)

# function to stamp files with the time of submission
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

# footer disclaimer code
footer <- tags$footer("Beta version: Send feedback to MarineGEO@si.edu", 
                                          align = "center", 
                                          style = "
                                          position:absolute;
                                          bottom:0;
                                          width:100%;
                                          height:20px; 
                                          color: black;
                                          background-color: yellow;
                                          z-index: 1000;"
)

# Authenticate your dropbox token 
drop_auth(rdstoken = "./droptoken.rds")



