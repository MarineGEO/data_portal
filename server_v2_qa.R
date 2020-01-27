# QA: Scans each upload - maintains link to original filename

# Create outputs: 
# ... List of output dataframes
# ... List of output directories
# ... List of output filenames
# ... List of ultimate QA pass/fail

# Save files 
# ... Ensure output directories exist
# ... Loop over output dataframes that pass QA, save with filename

# 1. New QA Process
# ... update draft QA to have only one QA table
# ... change submission_metadata$original to $original_filename
# ... change submission_metadata$new_filename to $new_filename
# 2. Split into outputs
# 3. Move determine output function
# 4. Organize error reporting
# 5. Update report 
# 6. Clean up submission metadata generation - collect sample collection dates to organize data (?)
# 7. Update submission log
# 8. Change directory to site-year

determineOutputs <- function(){
  # Cycle through each uploaded file to:
  # A. Get protocol name
  # B. determine which has multiple sites
  # C. Create list of output protocols
  index <- 1
  
  for(i in 1:length(submission_metadata$new_filename)){
    if(submission_metadata$site[i] != "multiple"){
      output_metadata$protocol[[index]] <- submission_metadata$protocol[i] 
      output_metadata$filename_new[[index]] <- submission_metadata$new_filename[i]
      output_metadata$filename_original[[index]] <- submission_metadata$original_filename[i]
      output_metadata$year[[index]] <- submission_metadata$year[i]
      output_metadata$site[[index]] <- submission_metadata$site[i]
      output_metadata$data_entry_date[[index]] <- submission_metadata$data_entry_date[i]
      output_metadata$protocol_df[[index]] <- submission_metadata$protocol_df[i][[1]]
      index <- index + 1
      
    } else {
      for(j in 1:length(unlist(submission_metadata$all_sites[i]))){
        output_metadata$protocol[[index]] <- submission_metadata$protocol[i] 
        output_metadata$filename_new[[index]] <- submission_metadata$new_filename[i]
        output_metadata$filename_original[[index]] <- submission_metadata$original[i]
        output_metadata$year[[index]] <- submission_metadata$year[i]
        output_metadata$site[[index]] <- unlist(submission_metadata$all_sites[i])[j]
        output_metadata$data_entry_date[[index]] <- submission_metadata$data_entry_date[i]
        output_metadata$protocol_df[[index]] <- submission_metadata$protocol_df[i][[1]]
        index <- index + 1
      }
    }
  }
}

testQA <- function(){
  
  # Data that passes QA tests will be saved to submission_data$all_data
  # It is a list of lists, where each first order list is a sheet in a protocol
  # And each second order list represents a protocol
  
  # Loop through each uploaded protocol
  for(i in 1:length(submission_metadata$original_filename)){
    
    current_protocol <- submission_metadata$protocol[i]
    
    # Get names of sheets for given protocol
    protocol_sheets <- protocol_structure %>%
      filter(protocol == current_protocol) %$% # Note use of %$% rather than %>%, allows you to use $ in unique and get results as a vector
      unique(.$sheet)
    
    # Create an empty list, each object will be a sheet for the protocol
    protocol_df <- vector("list", length(protocol_sheets))
    names(protocol_df) <- protocol_sheets
    
    # Read in each sheet for the protocol, assign to respective list object 
    for(sheet_name in protocol_sheets) {
      protocol_df[[sheet_name]] <- read_excel(output_metadata$filename_new[i], 
                                              sheet = sheet_name, 
                                              na = c("NA", "This cell will autocalculate", "N/A"))
    }
    
    ## TEST numeric type 
    # Get vector of numeric and integer type columns in the given protocol
    numeric_columns <- protocol_structure %>%
      filter(protocol == current_protocol) %>%
      filter(type == "numeric" | type == "integer") %$%
      unique(.$attribute_name)
    
    for(sheet_name in protocol_sheets){
      
      # Extract vector of numeric columns in sheet
      sheet_numeric_columns <- subset(colnames(protocol_df[[sheet_name]]), 
                                      colnames(protocol_df[[sheet_name]]) %in% numeric_columns)
      
      # If a sheet has numeric columns, attempt to convert them to numeric
      # If they have to coerce values to NA, the resulting warning will be logged 
      if(!is.null(sheet_numeric_columns)){
        tryCatch({
          
          protocol_df[[sheet_name]] <- protocol_df[[sheet_name]] %>%
            mutate_at(sheet_numeric_columns, as.numeric)
          
          # Remove any row that is all NAs - possible due to the autocalculation values
          # THIS NEEDS TO BE CHANGED IN THE FUTURE - only one column filtered on, will lead to problems
          if("sample_collection_date" %in% colnames(protocol_df[[sheet_name]])){
            protocol_df[[sheet_name]] <- filter(protocol_df[[sheet_name]], !is.na(sample_collection_date))
          }
          
          QA_results$df[nrow(QA_results$df) + 1,] <- c("Test numeric variables", 
                                                       output_metadata$filename_original[i], 
                                                       current_protocol, 
                                                       sheet_name, 
                                                       output_metadata$site[i], 
                                                       "Passed")
        },
        warning = function(w){
          QA_results$df[nrow(QA_results$df) + 1,] <<- c("Test numeric variables", 
                                                        output_metadata$filename_original[i], 
                                                        current_protocol, 
                                                        sheet_name, 
                                                        output_metadata$site[i], 
                                                        unlist(w[1]))
        })
      }
    }
    
    # Add the protocol to the overall submission data list 
    submission_data$all_data[[paste(current_protocol, 
                                    output_metadata$filename_original[i], 
                                    output_metadata$site[i], sep="_")]] <- protocol_df
    
    # Set success of protocol submission 
    current_results <- QA_results$df %>%
      filter(filename == output_metadata$filename_original[i] & site == output_metadata$site[i])
    
    if(all(current_results$status == "Passed")){
      output_metadata$status[i] <- TRUE
      # Currently everything is saved to curated directory
    } else output_metadata$status[i] <- TRUE
    
  }
  
  QA_results$summary <- QA_results$df %>%
    group_by(filename, site) %>%
    summarize(protocol = first(protocol), 
              result = ifelse(any(status != "Passed"), "Submission did not pass all tests", "Submission passed"))
  
}

## Code for filtering by site:
# if("site_code" %in% colnames(protocol_df[[sheet_name]])){
#   
#   # Line of code protects submission in case there is a typo between site code in sample metadata and site code in data
#   if(output_metadata$site[i] %in% unique(protocol_df[[sheet_name]]$site_code)){
#     protocol_df[[sheet_name]] <- protocol_df[[sheet_name]] %>%
#       filter(site_code == output_metadata$site[i])
#   }
# }
