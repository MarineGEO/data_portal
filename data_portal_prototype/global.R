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

# Read in protocol structure table
protocol_structure <- read_csv("./data/protocol_structure.csv")

# Record working directory to return to after moving to temporary directory
original_wd <- getwd()

