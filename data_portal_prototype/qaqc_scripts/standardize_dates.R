# convert date formats to proper format
# This function should accept ANY possible valid date format
library(tidyverse)
library(lubridate)
library(anytime)
library(readxl)

df <- read_xlsx("./documents/test_data/date_testing.xlsx", na="NA")

standardizeDates <- function(x){
  tryCatch({
    
    # Numeric check
    if(is.numeric(x) & 
       all(x < 47500, na.rm=T) & # Corresponds to any date < 2030
       all(x > 36500, na.rm=T)){ # Corresponds to any date > 2000
      
      return(as.Date(x, origin = "1899-12-30"))
      
    } else if (is.character(x)){
      # If running anydate returns a vector of all NA, likely is DD-MM-YYYY
      if(all(is.na(anydate(x)))){
        
      }
    }
    
  },
  error = function(e){
   
  })
}

standardizeDates(df$date_numeric)

# DD-MM-YYYY # Read in as character, excel will not convert to numeric # anydate converts to NA
anydate(df$chr_dmy)
# All NAs # Results in error 
anydate(df$empty_date)
# Some dates empty, DD-MM-YYYY # All NA
# Some empty, read in as date # Properly runs, NA stay as NA
anydate(df$some_empty)
# numeric Excel date, converts to wrong date
anydate(df$date_numeric)
# correct string format, date type, formats correctly
anydate(df$correct_format)

# Before using anydate, need to check
# ... Entire row isn't NA
# ... String isn't DD-MM-YYYY
# ... Isn't excel's date numeric format

if(is.numeric(df$date_numeric) & 
   all(df$date_numeric < 47500, na.rm=T) & # Corresponds to any date < 2030
   all(df$date_numeric > 36500, na.rm=T)){ # Corresponds to any date > 2000
  as.Date(df$date_numeric, origin = "1899-12-30")
}

df %>%
  select(chr_dmy) %>%
  separate(chr_dmy, into=c("first", "second", "third"), sep =  "[^0-9.]")

df %>%
  select(text) %>%
  separate(text, into=c("first", "second", "third"), sep =  "[^0-9.]")
