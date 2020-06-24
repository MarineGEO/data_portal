#library(tidyverse)
#library(magrittr)

# protocol_structure <- read_csv("./data_portal_prototype/data/protocol_structure.csv")
# current_protocol <- "seagrass_density"
# protocol_sheets <- c("sample_metadata", "sample_data", "taxa_list")

# Adjustments for shiny
# current protocol and protocol sheets to reactive
# stored_protocol to stored_protocol$df

## script containing functions to evaluate schema

## Check validity of table (Excel sheet) names #####
schemaTableNames <- function(){
  
  # Create empty data.frame to hold results
  table_results <- tibble(test = as.character(NA),
                          sheet_name = as.character(NA),
                          .rows = 0)
  
  # Get the list of table names that should be present in the current protocol
  schema_tables <- protocol_structure %>%
    filter(protocol == current_protocol()) 
  
  # Find absent tables (sheets missing from Excel upload) 
  absent_tables <-  schema_tables %>%
    filter(!(sheet %in% protocol_sheets())) %$%
    unique(.$sheet)
  
  # Find invalid tables (tables that aren't in the current schema)
  invalid_tables <- protocol_sheets()[!(protocol_sheets() %in% schema_tables$sheet)]
  print("here")
  # Construct results 
  if(length(absent_tables) >= 1){
    table_results <- table_results %>%
      add_row(test = "Absent table/sheet names",
              sheet_name = absent_tables)
  }
  
  if(length(invalid_tables) >= 1){
    table_results <- table_results %>%
      add_row(test = "Invalid table/sheet names",
              sheet_name = invalid_tables)
  }
  
  if(nrow(table_results) > 0){
    table_results <- table_results %>%
      mutate(column_name = NA,
             protocol = current_protocol(),
             filename = original_filename_qa(),
             values = NA,
             row_numbers = NA) %>%
      select(test, filename, protocol, sheet_name, column_name, row_numbers, values)
  }
  
  return(table_results)
}

## Check validity of column names #####

schemaColumnNames <- function(x){
  # Create empty data.frame to hold results
  column_results <- tibble(test = as.character(NA),
                           sheet_name = as.character(NA),
                           column_name = as.character(NA),
                           .rows = 0)
  
  for(table in protocol_sheets()){
    
    # Only check column names if the sheet/table name is present in the upload
    if(table %in% schema_tables$sheet){
      
      # Obtain any missing column names from data
      absent_columns <- schema_tables %>%
        filter(sheet == table) %>%
        filter(!(attribute_name %in% colnames(stored_protocol$df[[table]]))) %$%
        unique(.$attribute_name)
      
      # Determine which columns have been added by the submitter 
      column_names <- colnames(stored_protocol$df[[table]])
      invalid_columns <- column_names[!(column_names %in% schema_tables$attribute_name)]
      
      # Construct results 
      if(length(absent_columns) >= 1){
        column_results <- column_results %>%
          add_row(test = "Absent columns",
                  sheet_name = table,
                  column_name = absent_columns)
      }
      
      if(length(invalid_columns) >= 1){
        column_results <- column_results %>%
          add_row(test = "Invalid columns",
                  sheet_name = table,
                  column_name = invalid_columns)
      }
    }
  }
  
  print("here")
  if(nrow(column_results) > 0){
    column_results <- column_results %>%
      mutate(row_numbers = NA,
             protocol = current_protocol(),
             filename = original_filename_qa(),
             values = NA) %>%
      select(test, filename, protocol, sheet_name, column_name, row_numbers, values)
  }
  
  return(column_results)
}


## Check validity of values ####

