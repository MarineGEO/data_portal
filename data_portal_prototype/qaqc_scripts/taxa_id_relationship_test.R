## Test validity of Taxa ID relationships
# Function is not concerned if there is a taxon defined in taxa list that isn't in data sheets
checkTaxaRelationships <- function(){
  
  protocol_id_results <- data.frame()
  
  tryCatch({

    ## Check Taxa List ID relationships ####
    if("taxa_list" %in% protocol_sheets()){
      
      # Get unique ID variables in sample metadata sheet
      taxa_list <- stored_protocol$df$taxa_list
      
      # Check each data sheet to make sure unique combination of IDs are found in sample metadata page
      for(sheet_name in protocol_sheets()[!protocol_sheets() %in% c("sample_metadata", "taxa_list")]){
        
        if("taxon_id" %in% colnames(stored_protocol$df[[sheet_name]])){
          
          # anti_join with data sheet on left side
          # If there are rows, combine IDs for alert - represent taxon id values that don't exist in the taxa list sheet
          
          testing_df <- stored_protocol$df[[sheet_name]] %>%
            rowid_to_column("row")
          
          results1 <- anti_join(testing_df, taxa_list, by="taxon_id") %>%
            select(taxon_id, row)
          
          if(nrow(results1)>0){
            protocol_id_results <- results1 %>%
              # gather("column_name", "value", -taxon_id) %>%
              # group_by(column_name, value) %>%
              group_by(taxon_id) %>%
              summarize(row_numbers = paste(row, collapse=", ")) %>%
              mutate(sheet_name = sheet_name,
                     protocol = current_protocol(),
                     test = "Taxon ID value in sample data not defined",
                     filename = original_filename_qa(),
                     column_name = "taxon_id") %>%
              rename(values = taxon_id) %>%
              select(test, filename, protocol, sheet_name, column_name, values, row_numbers) %>%
              filter(!is.na(values)) %>%
              bind_rows(protocol_id_results)
          }
        }
      }
    }
    return(protocol_id_results)
    
  },
  
  error = function(e){
    print(e)
    
    # Create and return an error message in the QA result log 
    setNames(as.data.frame("Error checking taxa relationships"), "test") %>%
      mutate(column_name = NA,
             sheet_name = NA,
             protocol = current_protocol(),
             filename = original_filename_qa(),
             values = NA,
             row_numbers = NA) %>%
      select(test, filename, protocol, sheet_name, column_name, row_numbers, values) 
    
  })
  
  
}
