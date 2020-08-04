## Numeric variable tests ####
# Make sure values are all either NA or numeric

# For all QA scripts:
# ... Manipulate a list protocol list object, where each item in the list is a data frame, representing a sheet from the protocol (stored_protocol$df)
# ... The name of the protocol can be accessed with current_protocol()
# ... The name of the sheets in the protocol can be accessed with protocol_sheets()
# ... The name of the filename can be accessed with original_filename_qa()
# ... Each returns a standard QA dataframe with the following columns: 
# ... ... "test", "filename", "protocol", "sheet_name", "column_name", "row_numbers", "values"

testNumericType <- function(){

  tryCatch({
    numeric_columns <- protocol_structure %>%
      filter(protocol == current_protocol()) %>%
      filter(type == "numeric" | type == "integer") %$%
      unique(.$attribute)
    
    for(sheet_name in protocol_sheets()){
      
      # Extract vector of numeric columns in sheet
      sheet_numeric_columns <- subset(colnames(stored_protocol$df[[sheet_name]]),
                                      colnames(stored_protocol$df[[sheet_name]]) %in% numeric_columns)
      
      ## ... Test for non-numeric values ####
      # If a sheet has numeric columns, attempt to convert them to numeric
      # If they have to coerce values to NA, the resulting warning will be logged
      if(!is.null(sheet_numeric_columns) & nrow(stored_protocol$df[[sheet_name]]) != 0){
        
        testing_df <- stored_protocol$df[[sheet_name]] %>%
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
          numeric_results <- results %>%
            mutate(column = colnames(testing_df[col]),
                   row = row + 1) %>%
            group_by(column) %>%
            summarize(rows = paste(row, collapse=", ")) %>%
            mutate(sheet = sheet_name,
                   protocol = current_protocol(),
                   test = "Invalid characters in numeric attribute",
                   filename = original_filename_qa())
          
          QA_results$df <- QA_results$df %>%
            bind_rows(numeric_results)
        }
        
      }
    }

  },
  error = function(e){
    print(e)
    
    # Create and return an error message in the QA result log 
    QA_results$df <- QA_results$df %>%
      add_row(test = "Error in numeric data type test",
              protocol = current_protocol(),
              filename = original_filename_qa())
    
  })
  
  
}
