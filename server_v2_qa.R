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
      submission_metadata$new_filename[[index]] <- submission_metadata$new_filename[i]
      output_metadata$filename_original[[index]] <- submission_metadata$original_filename[i]
      output_metadata$year[[index]] <- submission_metadata$year[i]
      output_metadata$site[[index]] <- submission_metadata$site[i]
      output_metadata$data_entry_date[[index]] <- submission_metadata$data_entry_date[i]
      output_metadata$protocol_df[[index]] <- submission_metadata$protocol_df[i][[1]]
      index <- index + 1
      
    } else {
      for(j in 1:length(unlist(submission_metadata$all_sites[i]))){
        output_metadata$protocol[[index]] <- submission_metadata$protocol[i] 
        submission_metadata$new_filename[[index]] <- submission_metadata$new_filename[i]
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
  
  # Data will be read in, tested, and stored in submission_data$all_data
  # Submission_data$all_data is a list of lists, where each first order list is a sheet in a protocol
  # And each second order list represents a protocol
  
  # Store QA results in a data frame 
  QA_results <- setNames(data.frame(matrix(ncol = 7, nrow = 0)), 
                         c("test", "filename", "protocol", "sheet_name", "column_name", "row_numbers", "values"))
  
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
      df <- read_excel(submission_metadata$new_filename[i], 
                       sheet = sheet_name, 
                       na = c("NA", "This cell will autocalculate", "N/A"))
      
      # Need to prevent empty sheets from getting uploaded
      # For a sheet with no rows, no data frame will be associated at that branch of the list and the sheet name won't be in the testing list
      # TO DO - Add entry to QA_results in else statement
      if(nrow(df) > 0){
        protocol_df[[sheet_name]] <- df
      } else{
        protocol_sheets <- protocol_sheets[protocol_sheets != sheet_name]
      }
    }
    
    # Run  QA tests
    QA_results <- QA_results %>%
      bind_rows(checkSampleMetadata()) %>%
      bind_rows(checkIDRelationships()) %>%
      bind_rows(numericTests())
    
    # Add the protocol to the overall submission data list 
    submission_data$all_data[[paste(current_protocol, 
                                    submission_metadata$filename_original[i], 
                                    submission_metadata$site[i], sep="_")]] <- protocol_df
    
    # Set success of protocol submission 
    current_results <- QA_results$df %>%
      filter(filename == submission_metadata$filename_original[i] & site == submission_metadata$site[i])
    
    if(all(current_results$status == "Passed")){
      submission_metadata$status[i] <- TRUE
      # Currently everything is saved to curated directory
    } else submission_metadata$status[i] <- TRUE
  }
}

## Code for filtering by site:
# if("site_code" %in% colnames(protocol_df[[sheet_name]])){
#   
#   # Line of code protects submission in case there is a typo between site code in sample metadata and site code in data
#   if(submission_metadata$site[i] %in% unique(protocol_df[[sheet_name]]$site_code)){
#     protocol_df[[sheet_name]] <- protocol_df[[sheet_name]] %>%
#       filter(site_code == submission_metadata$site[i])
#   }
# }
