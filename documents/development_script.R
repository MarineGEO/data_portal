# Script used to test QA/QC functions/workflows independent of Shiny

library(tidyverse)
library(readxl)

# Format metadata to be more machine-readable
# Turn to wide form, remove first row
protocol_metadata <- read_xlsx("./documents/test.xlsx", sheet = "protocol_metadata", col_names = c("category", "response"), skip=1) %>%
  spread(category, response) %>%
  mutate(data_entry_date = as.Date(as.numeric(data_entry_date), origin = "1899-12-30"))

sample_metadata <- read_xlsx("./documents/test.xlsx", sheet = "sample_metadata")

# file name will be [Protocol]_[MarineGEO site code]_[data entry date in YYYY-MM-DD format]
file_name <- paste(protocol_metadata$protocol_name, first(sample_metadata$site_code), protocol_metadata$data_entry_date, sep="_")

# Make sure there's only one site code in sample metadata 
# Scan directory to make sure there isn't a shared name

# If excel sheet was done on Mac, date may be wrong since origin is different

# QA functions 
library(tidyverse)
library(readxl)
library(magrittr)

#test <- read_xlsx("./documents/testerror.xlsx", sheet = "shoot_count_sample_data")

#is.numeric(test$fruits_count)
#as.numeric(test$fruits_count)

protocol_structure <- read_csv("./documents/protocol_structure.csv")

current_protocol <- "seagrass_density"

# Get names of sheets in given protocol
protocol_sheets <- protocol_structure %>%
  filter(protocol == current_protocol) %$% # Note use of %$% rather than %>%, allows you to use $ in unique and get results as a vector
  unique(.$sheet)

# Create an empty list, each object will be a sheet for the protocol
protocol_df <- vector("list", length(protocol_sheets))
names(protocol_df) <- protocol_sheets

# Read in each sheet for the protocol, assign to respective list object 
for(sheet_name in protocol_sheets) {
  protocol_df[[sheet_name]] <- read_excel("./documents/testerror.xlsx", sheet = sheet_name)
}

# QA test 
# Test if columns that should be numeric are numeric
# If not, try to convert

# Get vector of numeric type columns in the given protocol
numeric_columns <- protocol_structure %>%
  filter(protocol == current_protocol) %>%
  filter(type == "numeric") %$%
  unique(.$attribute_name)

# Create object to save errors to 
QA_results <- setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("test", "protocol", "sheet_name", "error_message"))

for(sheet_name in protocol_sheets){
  
  # Extract vector of numeric columns in sheet
  sheet_numeric_columns <- subset(colnames(protocol_df[[sheet_name]]), 
                                  colnames(protocol_df[[sheet_name]]) %in% numeric_columns)
  
  # Select sheet dataframe to the subset. 
  if(!is.null(sheet_numeric_columns)){
    tryCatch({
      protocol_df[[sheet_name]] <- protocol_df[[sheet_name]] %>%
        mutate_at(sheet_numeric_columns, as.numeric)
      
      QA_results[nrow(QA_results) + 1,] <- c("Test numeric variables", current_protocol, sheet_name, "Passed")
    },
    
    warning = function(w){
      QA_results[nrow(QA_results) + 1,] <<- c("Test numeric variables", current_protocol, sheet_name, unlist(w[1]))
    })
  }
  
}