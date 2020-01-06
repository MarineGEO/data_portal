library(tidyverse)
library(magrittr)
library(readxl)

# Tests
# Check for empty rows & remove
# ... Check if ALL rows are empty
# Ensure site codes match
# As.numeric test

# Test data for QA/QC beta
protocol_structure <- read_csv("./data_portal_prototype/data/protocol_structure.csv")

filenames <- c("fish_seines_USA-VASB_2019-09-23.xlsx",
               "seagrass_density_USA-VASB_2019-09-23.xlsx",
               "seagrass_epifauna_USA-VASB_2019-09-23.xlsx",
               "seagrass_shoots_USA-VASB_2019-09-23.xlsx")

protocols <- c("fish_seines", "seagrass_density", "seagrass_epifauna", "seagrass_shoots")

sites <- c("USA-VASB", "USA-VASB", "USA-VASB", "USA-VASB")

# Create table to hold QA results
QA_results <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("test", "filename", "protocol", "sheet_name", "column_name", "comments"))
QA_summary <- data.frame()

# list will hold all imported data
all_data = list()

for(i in 1:length(filenames)){
  
  current_protocol <- protocols[i]
  
  # Get names of sheets for given protocol
  protocol_sheets <- protocol_structure %>%
    filter(protocol == current_protocol) %$% # Note use of %$% rather than %>%, allows you to use $ in unique and get results as a vector
    unique(.$sheet)
  
  # Create an empty list, each object will be a sheet for the protocol
  protocol_df <- vector("list", length(protocol_sheets))
  names(protocol_df) <- protocol_sheets
  
  # Read in each sheet for the protocol, assign to respective list object 
  for(sheet_name in protocol_sheets) {
    protocol_df[[sheet_name]] <- read_excel(paste("./documents/secrets", filenames[i], sep="/"), 
                                            sheet = sheet_name, 
                                            na = c("NA", "This cell will autocalculate", "N/A"))
  }
  
  # Run QA tests
  checkIDRelationships()
  #numericTests()
  
  # # Add the protocol to the overall submission data list 
  all_data[[paste(current_protocol,
                  filenames[i],
                  sites[i], sep="_")]] <- protocol_df
  
  
}

checkIDRelationships <- function(){
  
  # Get unique ID variables in sample metadata sheet
  sample_metadata_ids <- protocol_structure %>%
    filter(protocol == current_protocol & sheet = "sample_metadata") %>%
    filter(id_variable == 1) %$%
    unique(.$attribute_name)
  
  # Check each data sheet to make sure unique combination of IDs are found in sample metadata page
  for(sheet_name in protocol_sheets[!protocol_sheets %in% c("sample_metadata", "taxa_list")]){
    
    # Extract IDs in data sheet that can be found in sample metadata sheet
    data_ids <- protocol_structure %>%
      filter(protocol == current_protocol & sheet = sheet_name) %>%
      filter(id_variable == 1) %>%
      filter(attribute_name %in% sample_metadata_ids) %$%
      unique(.$attribute_name)
    
    # anti_join with data sheet on left side
    # If there are rows, combine IDs for alert
    # Unite all values
    
    test <- anti_join(protocol_df[[sheet_name]], protocol_df$sample_metadata, by=data_ids) %>%
      unite(data_ids, id)
      
    if(nrow(test) > 0){
      QA_results <- results %>%
        mutate(id_links = colnames(testing_df[col])) %>%
        group_by(column_name) %>%
        summarize(comments = paste0("Check the following row numbers: ", paste(row, collapse=", "))) %>%
        mutate(sheet_name = sheet_name,
               protocol = current_protocol,
               test = "Test numeric variables",
               filename = filenames[i]) %>%
        select(test, filename, protocol, sheet_name, column_name, comments) %>%
        bind_rows(QA_results)
    }
  }
  
  # First do anti_join with data on left side
  # If there are rows, combine IDs for alert
  
  # Second, do opposite anti join
  # Combine IDs for alerts
  
  # If none of these provide alerts, done
  # But if so, run tests for each ID category
  # Make sure all data sheets are found in sample metadata, check for any NAs

  protocol_df$sample_metadata 
  
}

## TEST numeric type 
# Get vector of numeric and integer type columns in the given protocol
numericTests <- function(){
  numeric_columns <- protocol_structure %>%
    filter(protocol == current_protocol) %>%
    filter(type == "numeric" | type == "integer") %$%
    unique(.$attribute_name)
  
  for(sheet_name in protocol_sheets){
    
    # Extract vector of numeric columns in sheet
    sheet_numeric_columns <- subset(colnames(protocol_df[[sheet_name]]),
                                    colnames(protocol_df[[sheet_name]]) %in% numeric_columns)
    
    # If a sheet has numeric columns, attempt to convert them to numeric
    # If they have to coerce values to NA, the resulting warning will be logged
    if(!is.null(sheet_numeric_columns) & nrow(protocol_df[[sheet_name]]) != 0){
      
      testing_df <- protocol_df[[sheet_name]] %>%
        select(sheet_numeric_columns)
      
      numeric_test <- as.data.frame(
        apply(testing_df, c(1,2), function(x){
          tryCatch({
            value <- as.numeric(x)
            TRUE
          },
          warning = function(w){
            FALSE
          })
        }))
      
      results <- as.data.frame(which(!numeric_test, arr.ind=TRUE)) 
      
      if(nrow(results)>0){
        QA_results <- results %>%
          mutate(column_name = colnames(testing_df[col])) %>%
          group_by(column_name) %>%
          summarize(row_numbers = paste(row, collapse=", ")) %>%
          mutate(sheet_name = sheet_name,
                 protocol = current_protocol,
                 test = "Test numeric variables",
                 filename = filenames[i]) %>%
          select(test, filename, protocol, sheet_name, column_name, row_numbers) %>%
          bind_rows(QA_results)
      }
    }
  }
}

