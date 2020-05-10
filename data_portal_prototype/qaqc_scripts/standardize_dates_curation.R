# convert date formats to proper format
# This function should accept ANY possible valid date format
# Use of anydate() function automatically converts to correct format (YYYY-MM-DD) when possible 

standardizeDates <- function(x){
  results <- data.frame()
  
  tryCatch({
    # Pull out any attribute names in protocol that are Date type
    date_columns <- protocol_structure %>%
      filter(protocol == current_protocol()) %>%
      filter(type == "Date") %$%
      unique(.$attribute_name)
    
    for(sheet_name in protocol_sheets()){
      
      # Extract vector of numeric columns in sheet
      sheet_date_columns <- subset(colnames(stored_protocol$df[[sheet_name]]),
                                   colnames(stored_protocol$df[[sheet_name]]) %in% date_columns)
      
      # If the sheet has date columns and rows with date, proceed with test
      if(!is.null(sheet_date_columns) & nrow(stored_protocol$df[[sheet_name]]) != 0){
        
        # For each date column, standardize dates
        for(date_attribute in sheet_date_columns){
          
          # Save column as X for brevity
          x <- stored_protocol$df[[sheet_name]][[date_attribute]]
          # Check if the date is in the Excel date-number format and attempt to convert
          if(is.numeric(x) & 
             all(x < 47500, na.rm=T) & # Corresponds to any date < 2030
             all(x > 36500, na.rm=T)){ # Corresponds to any date > 2000
            
            stored_protocol$df[[sheet_name]][[date_attribute]] <- as.Date(x, origin = "1899-12-30")
            
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
              
              if(sum(is.na(reformat$new_date) <= num_na_original)) {
                stored_protocol$df[[sheet_name]][[date_attribute]] <- reformat$new_date
              
              } else {
                # record inability to convert without loss of information 
                results <- setNames(as.data.frame("Loss of data in date conversion"), "test") %>%
                  mutate(column_name = date_attribute,
                         sheet_name = sheet_name,
                         protocol = current_protocol(),
                         filename = original_filename_qa()) %>%
                  select(test, filename, protocol, sheet_name, column_name) %>%
                  bind_rows(results)
              }
              
              # No loss of information, data was likely in appropariate format already
            } else {
              stored_protocol$df[[sheet_name]][[date_attribute]] <- y
            }
            
            
          } else if (is.Date(x) | is.POSIXt(x)){
            
            stored_protocol$df[[sheet_name]][[date_attribute]] <- as.Date(x)
            
          } else {
            # record inability to convert due to unknown typing
            results <- setNames(as.data.frame("Unknown date format"), "test") %>%
              mutate(column_name = date_attribute,
                     sheet_name = sheet_name,
                     protocol = current_protocol(),
                     filename = original_filename_qa()) %>%
              select(test, filename, protocol, sheet_name, column_name) %>%
              bind_rows(results)
          }
          
        }
      }
    }
    return(results)
  },
  
  error = function(e){
    # Create and return an error message in the QA result log 
    setNames(as.data.frame("Unknown error in date standardization"), "test") %>%
      mutate(column_name = NA,
             sheet_name = NA,
             protocol = current_protocol(),
             filename = original_filename_qa(),
             values = NA,
             row_numbers = NA) %>%
      select(test, filename, protocol, sheet_name, column_name, row_numbers, values)
  })
}


## Code for testing function outside of application #### 
# standardizeDates <- function(x){
#   tryCatch({
#     
#     # Check if the date is in the Excel date-number format and attempt to convert
#     if(is.numeric(x) & 
#        all(x < 47500, na.rm=T) & # Corresponds to any date < 2030
#        all(x > 36500, na.rm=T)){ # Corresponds to any date > 2000
#       
#       return(as.Date(x, origin = "1899-12-30"))
#       
#     } else if (is.character(x)){
#       # First count the number of NAs already present
#       num_na_original <- sum(is.na(x))
#       # Attempt anydate conversion
#       y <- anydate(x)
#       
#       # If the number of NAs after conversion is greater than before, 
#       # date format may be in DD-MM-YYYY or improper character strings are present
#       if(sum(is.na(y)) > num_na_original){
#         
#         # Separate date into three columns based on delimiter
#         # Rearrange to YYYY-MM-DD and convert using anydate()
#         results <- separate(as.data.frame(x), x, into=c("first", "second", "third"), sep =  "[^0-9.]") %>%
#           mutate(new_date = paste(third, second, first, sep="-")) %>%
#           mutate(new_date = anydate(new_date)) 
#         
#         if(sum(is.na(results$new_date) <= num_na_original)) {
#           return(results$new_date)
#         } else {
#           # record inability to convert without loss of information 
#           
#           
#           return(x)
#         }
#         
#       } else {
#         return(y)
#       }
#       
#       
#     } else if (is.Date(x) | is.POSIXt(x)){
#       
#       return(anydate(x))
#     } else {
#       # record inability to convert due to unknown typing
#       return(x)
#     }
#     
#   },
#   error = function(e){
#     
#   })
# }

# df <- read_xlsx("./documents/test_data/date_testing.xlsx", na="NA")
# 
# # DD-MM-YYYY # Read in as character, excel will not convert to numeric # anydate converts to NA
# anydate(df$chr_dmy)
# standardizeDates(df$chr_dmy) # Converts correctly, all days > 12
# standardizeDates(df$chr_dmy_mixed) # Converts correctly, mix of days > and < 12
# 
# # All NAs # Results in error 
# anydate(df$empty_date)
# standardizeDates(df$empty_date) # Returns nothing 
# 
# # Some dates empty, DD-MM-YYYY # All NA
# # Some empty, read in as date # Properly runs, NA stay as NA
# anydate(df$some_empty)
# standardizeDates(df$some_empty) # Converts correctly
# 
# # numeric Excel date, converts to wrong date
# anydate(df$date_numeric)
# standardizeDates(df$date_numeric) # Converts correctly
# 
# # correct string format, date type, formats correctly
# anydate(df$correct_format)
# standardizeDates(df$correct_format)
# 
# # text strings
# standardizeDates(df$text) # returns all NAs, generates warning in separate call
