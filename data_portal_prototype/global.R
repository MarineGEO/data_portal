# MarineGEO data submission app example 
# This is the global script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu

# Load necessary packages
library(shiny)
library(shinyjs)
library(readxl)
library(tidyverse)
library(rdrop2)

# function to stamp files with the time of submission
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

