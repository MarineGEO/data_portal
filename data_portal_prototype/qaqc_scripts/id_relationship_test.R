## Test validity of ID variables ####
checkIDRelationships <- function(){
  protocol_id_results <- data.frame()
  
  tryCatch({
  ## Match IDs across sheets #####
  # Get unique ID variables in sample metadata sheet
  sample_metadata_ids <- protocol_structure %>%
    filter(protocol == current_protocol() & sheet == "sample_metadata") %>%
    filter(id_variable == 1) %$%
    unique(.$attribute_name)
  
  # Check each data sheet to make sure unique combination of IDs are found in sample metadata page
  for(sheet_name in protocol_sheets()[!protocol_sheets() %in% c("sample_metadata", "taxa_list")]){
    
    # Extract IDs in data sheet that can be found in sample metadata sheet
    data_ids <- protocol_structure %>%
      filter(protocol == current_protocol() & sheet == sheet_name) %>%
      filter(id_variable == 1) %>%
      filter(attribute_name %in% sample_metadata_ids) %$%
      unique(.$attribute_name)
    
    # anti_join with data sheet on left side
    # If there are rows, combine IDs for alert - represent metadata values that don't exist in the sample metadata page
    
    for(id in data_ids){
      testing_df <- stored_protocol$df[[sheet_name]] %>%
        rowid_to_column("row")
      
      results1 <- anti_join(testing_df, stored_protocol$df$sample_metadata, by=id) %>%
        select(id, row)
      
      if(nrow(results1)>0){
        protocol_id_results <- results1 %>%
          gather("column_name", "value", -row) %>%
          group_by(column_name, value) %>%
          summarize(row_numbers = paste(row, collapse=", ")) %>%
          mutate(sheet_name = sheet_name,
                 protocol = current_protocol(),
                 test = "Invalid ID value in sample data",
                 filename = original_filename_qa(),
                 values = as.character(value)) %>%
          select(test, filename, protocol, sheet_name, column_name, values, row_numbers) %>%
          bind_rows(protocol_id_results)
      }
      
      testing_df <- stored_protocol$df$sample_metadata %>%
        rowid_to_column("row")
      
      results2 <- anti_join(testing_df, stored_protocol$df[[sheet_name]], by=id) %>%
        select(id, row)
      
      if(nrow(results2)>0){
        protocol_id_results <- results2 %>%
          gather("column_name", "value", -row) %>%
          group_by(column_name, value) %>%
          summarize(row_numbers = paste(row, collapse=", ")) %>%
          mutate(sheet_name = "sample_metadata",
                 protocol = current_protocol(),
                 test = "Invalid ID value in sample metadata",
                 filename = original_filename_qa(),
                 values = as.character(value)) %>%
          select(test, filename, protocol, sheet_name, column_name, values, row_numbers) %>%
          bind_rows(protocol_id_results)
        
      }
    }
  }
  return(protocol_id_results)
  
  },
  error = function(e){
    # Create and return error message in the QA result log 
    setNames(as.data.frame("Error testing ID relationships"), "test") %>%
      mutate(column_name = NA,
             sheet_name = NA,
             protocol = current_protocol(),
             filename = original_filename_qa(),
             values = NA,
             row_numbers = NA) %>%
      select(test, filename, protocol, sheet_name, column_name, row_numbers, values)
    
  })
  

}