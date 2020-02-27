## Tests for sample metadata sheet ####

checkSampleMetadata <- function(){
  sample_metadata_results <- data.frame()
  
  tryCatch({
    testing_df <- stored_protocol$df$sample_metadata 
    
    ## ... Test that site codes are in roster ####
    
    ## ... Test sample collection date ####
    
    if("sample_collection_date" %in% colnames(testing_df)){
      invalid_date_index <- which(is.na(anydate(testing_df$sample_collection_date)))
      
      if(length(invalid_date_index)>0){
        # Extract invalid value at each index
        invalid_values <- testing_df$sample_collection_date[invalid_date_index]
        
        sample_metadata_results <- setNames(as.data.frame(paste(invalid_date_index, collapse = ", ")), "row_numbers") %>%
          mutate(column_name = "sample_collection_date",
                 sheet_name = "sample_metadata",
                 protocol = current_protocol(),
                 test = "Invalid sample collection date format",
                 filename = original_filename_qa(),
                 values = paste(invalid_values, collapse=", ")) %>%
          select(test, filename, protocol, sheet_name, column_name, row_numbers, values) %>%
          bind_rows(sample_metadata_results)
      }
      
    } 
    
    ## ... Check existance of coordinates ####
    # ID value 2 represents coordinate attributes (lat and long)
    coordinate_attributes <- protocol_structure %>%
      filter(protocol == current_protocol() & sheet == "sample_metadata") %>%
      filter(id_variable == 2) %$%
      unique(.$attribute_name)
    
    
    ## ... Convert DMS to DD #####
    
    return(sample_metadata_results)
    
  },
  error = function(e){
    print(e)
    
    # Create and return an error message in the QA result log 
    setNames(as.data.frame("Error in sample metadata tests"), "test") %>%
      mutate(column_name = NA,
             sheet_name = NA,
             protocol = current_protocol(),
             filename = original_filename_qa(),
             values = NA,
             row_numbers = NA) %>%
      select(test, filename, protocol, sheet_name, column_name, row_numbers, values)
    
  })
  
}
