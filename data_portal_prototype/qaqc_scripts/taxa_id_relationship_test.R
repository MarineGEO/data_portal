## Test validity of Taxa ID relationships
checkTaxaRelationships <- function(){
  protocol_id_results <- data.frame()
  
  ## Match IDs across sheets #####
  # Get unique ID variables in sample metadata sheet
  sample_metadata_ids <- protocol_structure %>%
    filter(protocol == current_protocol() & sheet == "sample_metadata") %>%
    filter(id_variable == 1) %$%
    unique(.$attribute_name)
  
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
            bind_rows(protocol_id_results)
        }
      }
    }
  }
  
  
  return(protocol_id_results)
}
