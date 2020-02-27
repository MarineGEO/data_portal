# For all QA scripts:
# ... Manipulate a list protocol list object, where each item in the list is a data frame, representing a sheet from the protocol (stored_protocol$df)
# ... The name of the protocol can be accessed with current_protocol()
# ... The name of the sheets in the protocol can be accessed with protocol_sheets()
# ... The name of the filename can be accessed with original_filename_qa()
# ... Each returns a standard QA dataframe with the following columns: 
# ... ... "test", "filename", "protocol", "sheet_name", "column_name", "row_numbers", "values"

## ... Test for valid minimum and maximum values ####
numericMinMaxTest <- function(){
  
  numeric_results <- data.frame()
  
  tryCatch({
    numeric_columns <- protocol_structure %>%
      filter(protocol == current_protocol()) %>%
      filter(type == "numeric" | type == "integer") %$%
      unique(.$attribute_name)
    
    for(sheet_name in protocol_sheets()){
      
      # Extract vector of numeric columns in sheet
      sheet_numeric_columns <- subset(colnames(stored_protocol$df[[sheet_name]]),
                                      colnames(stored_protocol$df[[sheet_name]]) %in% numeric_columns)
      
      if(!is.null(sheet_numeric_columns) & nrow(stored_protocol$df[[sheet_name]]) != 0){
        
        testing_df <- stored_protocol$df[[sheet_name]] %>%
          select(sheet_numeric_columns)
        
        protocol_minmax <- protocol_structure %>%
          filter(protocol == current_protocol() & sheet == sheet_name & attribute_name %in% colnames(testing_df))
        
        for(column in colnames(testing_df)){
          
          ## ... Minimum Test ####
          # Find the index for any values that are less than the attribute's minimum
          min_invalid_row_index <- which((testing_df[column] < filter(protocol_minmax, attribute_name == column)$minimum_warning) 
                                         %in% 1) 
          # If there are invalid values:
          if(length(min_invalid_row_index) > 0){
            # Extract invalid value at each index
            invalid_values <- pull(testing_df[column])[min_invalid_row_index]
            
            numeric_results <- setNames(as.data.frame(paste(min_invalid_row_index, collapse = ", ")), "row_numbers") %>%
              mutate(column_name = column,
                     sheet_name = sheet_name,
                     protocol = current_protocol(),
                     test = "Invalid minimum value",
                     filename = original_filename_qa(),
                     values = paste(invalid_values, collapse=", ")) %>%
              select(test, filename, protocol, sheet_name, column_name, row_numbers, values) %>%
              bind_rows(numeric_results)
          }
          
          ## ... Maximum Test ####
          # Find the index for any values that are greater than the attribute's maximum
          max_invalid_row_index <- which((testing_df[column] > filter(protocol_minmax, attribute_name == column)$maximum_warning)
                                         %in% 1) 
          # If there are invalid values:
          if(length(max_invalid_row_index) > 0){
            # Extract invalid value at each index
            invalid_values <- pull(testing_df[column])[max_invalid_row_index]
            
            numeric_results <- setNames(as.data.frame(paste(max_invalid_row_index, collapse = ", ")), "row_numbers") %>%
              mutate(column_name = column,
                     sheet_name = sheet_name,
                     protocol = current_protocol(),
                     test = "Invalid maximum value",
                     filename = original_filename_qa(),
                     values = paste(invalid_values, collapse=", ")) %>%
              select(test, filename, protocol, sheet_name, column_name, row_numbers, values) %>%
              bind_rows(numeric_results)
          }
        }
        
      }
    }
    return(numeric_results)
    
  },
  error = function(e){
    print(e)
    
    # Create and return an error message in the QA result log 
    setNames(as.data.frame("Error in min/max test"), "test") %>%
      mutate(column_name = NA,
             sheet_name = NA,
             protocol = current_protocol(),
             filename = original_filename_qa(),
             values = NA,
             row_numbers = NA) %>%
      select(test, filename, protocol, sheet_name, column_name, row_numbers, values)
    
  })
  
}