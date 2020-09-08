# Attempts to convert date formats to proper format
# This function provides information on what data format data is arriving in
# No data in submission is changed (see standardize_dates_curation.R)
# Use of anydate() function automatically converts to correct format (YYYY-MM-DD) when possible 

evaluateDates <- function(x){
  
  tryCatch({
    # Pull out any attribute names in protocol that are Date type
    date_columns <- protocol_structure %>%
      filter(protocol == current_protocol()) %>%
      filter(type == "date") %$%
      unique(.$attribute)
    
    for(sheet_name in protocol_sheets()){
      
      # Extract vector of numeric columns in sheet
      sheet_date_columns <- subset(colnames(stored_protocol$df[[sheet_name]]),
                                   colnames(stored_protocol$df[[sheet_name]]) %in% date_columns)
      
      # If the sheet has date columns proceed with test
      if(!is.null(sheet_date_columns)){
        
        ## Attempt to convert date columns and evaluate output ####
        
        # For each date column, standardize dates
        for(date_attribute in sheet_date_columns){
          
          # Save column as X for brevity
          x <- stored_protocol$df[[sheet_name]][[date_attribute]]
          # Check if the date is in the Excel date-number format and attempt to convert
          if(is.numeric(x) & 
             all(x < 47500, na.rm=T) & # Corresponds to any date < 2030
             all(x > 36500, na.rm=T)){ # Corresponds to any date > 2000
            
            QA_results$df <- QA_results$df %>%
              add_row(test = "Date is in numeric format",
                      column = date_attribute,
                      sheet = sheet_name,
                      protocol = current_protocol(),
                      filename = original_filename_qa(),
                      submission_id = current_submission_id())
                      
          } else if (is.character(x)){
            # First count the number of NAs already present
            num_na_original <- sum(is.na(x))
            # Attempt anydate conversion
            y <- anydate(x)
            
            # If the number of NAs after conversion is greater than before, 
            # date format may be in DD-MM-YYYY or improper character strings are present
            if(sum(is.na(y)) > num_na_original){
              
              # Separate date into three columns based on delimiter
              # Rearrange to YYYY-MM-DD and convert using anydate()
              reformat <- separate(as.data.frame(x), x, into=c("first", "second", "third"), sep =  "[^0-9.]") %>%
                mutate(new_date = paste(third, second, first, sep="-")) %>%
                mutate(new_date = anydate(new_date)) 
              
              if(sum(is.na(reformat$new_date) > num_na_original)) {
                
                # record inability to convert without loss of information 
                QA_results$df <- QA_results$df %>%
                  add_row(test = "Loss of data in data conversion (character)",
                          column = date_attribute,
                          sheet = sheet_name,
                          protocol = current_protocol(),
                          filename = original_filename_qa(),
                          submission_id = current_submission_id())
              }
            } 
            
          } else if (is.Date(x) | is.POSIXt(x)){
            

          } else {
            # record inability to convert due to unknown typing
            QA_results$df <- QA_results$df %>%
              add_row(test = "Unknown date format",
                      column = date_attribute,
                      sheet = sheet_name,
                      protocol = current_protocol(),
                      filename = original_filename_qa(),
                      submission_id = current_submission_id())
          }
        
          # Check for any NA dates in sample metadata table ####
          if(sheet_name == "sample_metadata"){

            # Pull out NA row indices
            invalid_date_index <- which(is.na(x))

            if(length(invalid_date_index)>0){

              # R row indices are 1 row less than Excel row indices
              # (row 2 in Excel is the same as row 1 in R dataframe)
              invalid_date_index <- invalid_date_index + 1

              QA_results$df <- QA_results$df %>%
                add_row(test = "Missing date information",
                        column = date_attribute,
                        sheet = sheet_name,
                        protocol = current_protocol(),
                        filename = original_filename_qa(),
                        rows = paste(invalid_date_index, collapse = ", "),
                        submission_id = current_submission_id())
            }

          }
        }
        
      }
    }
  },
  
  error = function(e){
    print(e)
    
    # Create and return an error message in the QA result log 
    QA_results$df <- QA_results$df %>%
      add_row(test = "Unknown error in date format test",
              protocol = current_protocol(),
              filename = original_filename_qa(),
              submission_id = current_submission_id())
  })
}

