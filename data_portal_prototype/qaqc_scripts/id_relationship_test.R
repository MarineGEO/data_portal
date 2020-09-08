## Test validity of ID variables ####
checkIDRelationships <- function(){
  # protocol_id_results <- data.frame()
  
  tryCatch({
  ## Match IDs across sheets #####
  # Get unique ID variables in sample metadata sheet
  sample_metadata_ids <- protocol_structure %>%
    filter(protocol == current_protocol() & table == "sample_metadata") %>%
    filter(id_variable == 1) %$%
    unique(.$attribute)
  
  # Check each data sheet to make sure unique combination of IDs are found in sample metadata page
  for(sheet_name in protocol_sheets()[!protocol_sheets() %in% c("sample_metadata", "taxa_list")]){
    
    # Extract IDs in data sheet that can be found in sample metadata sheet
    data_ids <- protocol_structure %>%
      filter(protocol == current_protocol() & table == sheet_name) %>%
      filter(id_variable == 1) %>%
      filter(attribute %in% sample_metadata_ids) %$%
      unique(.$attribute)
    
    # anti_join with data sheet on left side
    # If there are rows, combine IDs for alert - represent metadata values that don't exist in the sample metadata page
    
    for(id in data_ids){
      
      testing_df <- stored_protocol$df[[sheet_name]] %>%
        rowid_to_column("row")
      
      results1 <- anti_join(testing_df, stored_protocol$df$sample_metadata, by=id) %>%
        select(id, row) %>%
        mutate(row = row + 1) # Update row numbers to reflect Excel row numbers
      
      if(nrow(results1)>0){
        results1_formatted <- results1 %>%
          gather("column", "value", -row) %>%
          group_by(column, value) %>%
          summarize(rows = paste(row, collapse=", ")) %>%
          mutate(sheet = sheet_name,
                 protocol = current_protocol(),
                 test = "Invalid ID value in sample data",
                 filename = original_filename_qa(),
                 values = as.character(value),
                 submission_id = current_submission_id()) %>%
          select(-value)
        
        QA_results$df <- QA_results$df %>%
          bind_rows(results1_formatted)
      }
      
      testing_df <- stored_protocol$df$sample_metadata %>%
        rowid_to_column("row")
      
      results2 <- anti_join(testing_df, stored_protocol$df[[sheet_name]], by=id) %>%
        select(id, row) %>%
        mutate(row = row + 1) # Update row numbers to reflect Excel row numbers
      
      if(nrow(results2)>0){
        results2_formatted <- results2 %>%
          gather("column", "value", -row) %>%
          group_by(column, value) %>%
          summarize(rows = paste(row, collapse=", ")) %>%
          mutate(sheet = "sample_metadata",
                 protocol = current_protocol(),
                 test = "Invalid ID value in sample metadata",
                 filename = original_filename_qa(),
                 values = as.character(value),
                 submission_id = current_submission_id()) %>%
          select(-value)
        
        QA_results$df <- QA_results$df %>%
          bind_rows(results2_formatted)
      }
    }
  }
  },
  error = function(e){
    print(e)
    
    # Create and return an error message in the QA result log 
    QA_results$df <- QA_results$df %>%
      add_row(test = "Error testing ID relationships",
              protocol = current_protocol(),
              filename = original_filename_qa())
    
  })


}