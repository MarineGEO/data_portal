# This is a sub-script for server.R of the data submission portal application used by MarineGEO
# It contains functions to extract the site - data entry date - protocol name - protocol version metadata information from submissions
# It catches errors and provides warnings in case information is not formatted correctly or is missing
# Contact: Michael Lonneman (lonnemanM@si.edu) or email marinegeo@si.edu

# Each sub-function is embedded within one error catching function
# The main error would be the lack of a "protocol metadata" or "sample metadata" sheet in the protocols. 
extractProtocolMetadata <- function(){
  setwd(tempdir())
  
  # For each file uploaded: 
  for(i in 1:nrow(input$fileExcel)){
    
    # Name of the file as the user uploaded it
    original_filename <- input$fileExcel$name[i]
    
    ## Each sub-function has a unique error catcher ##
    # Read in protocol and sample metadata sheets
    protocol_metadata <- readProtocolMetadata(input$fileExcel$datapath[i], original_filename)
    sample_metadata <- readSampleMetadata(input$fileExcel$datapath[i], original_filename)

    ## Extract each piece of metadata needed to store submission ##
    submission_metadata$wb_version[i] <- extractWorkbookVersions(original_filename, protocol_metadata) 
    submission_metadata$protocol[i] <- extractProtocolName(original_filename, protocol_metadata) 
    
    # Check the number of sites - if more than one, data for each site will be filed separately. 
    submission_metadata$site[i] <- checkNumberOfSites(original_filename, sample_metadata)
    
    # If more than one site, list of sites created
    if(submission_metadata$site[i] == "multiple"){
      submission_metadata$all_sites[i] <- list(unique(sample_metadata$site_code)) 
    } else submission_metadata$all_sites[i] <- submission_metadata$site[i]
    
    extractProtocolDate()
    
  }
}

readProtocolMetadata <- function(filepath, original_filename){
  # Wrapped in an error catcher - if user does not upload an excel sheet with a protocol_metadata sheet, the submission will fail
  tryCatch({
    # Import data and format metadata to be more machine-readable
    # Turn to wide form, remove first row
    read_xlsx(filepath, 
              sheet = "protocol_metadata", 
              col_names = c("category", "response"), skip=1) %>%
      spread(category, response) 
  },
  error = function(e){
    # Track which file triggered the error and the cause (no protocol metadata sheet)
    protocol_metadata_error$df[nrow(protocol_metadata_error$df) + 1,] <-  c(original_filename, 
                                                                            filter(warnings, title == "protocol_metadata_error")$message)
  return(NA)
  })
}

readSampleMetadata <- function(filepath, original_filename){
  # Wrapped in an error catcher - if user does not upload an excel sheet with a sample_metadata sheet, the submission will fail
  tryCatch({
    # Read in sample metadata to get site code
    read_xlsx(filepath, sheet = "sample_metadata")
  },
  error = function(e){
    # Track which file triggered the error and the cause (no sample metadata sheet)
    protocol_metadata_error$df[nrow(protocol_metadata_error$df) + 1,] <-  c(original_filename,
                                                                            filter(warnings, title == "sample_metadata_error")$message)
  return(NA)
  })
}

extractWorkbookVersions <- function(original_filename, protocol_metadata){
  if(is.na(protocol_metadata)) return("invalid")
  
  # Wrapped in an error catcher - if there is no workbook version value present the submission will fail
  tryCatch({
    # Pull workbook version
    # If $workbook_version doesn't exist, will only generate a warning rather than an error
    version <- protocol_metadata$workbook_version
    
    if(version == "v0.3.0" | version == "v0.4.0"){
      return(version)
    # If an older spread is submitted, record the error  
    } else {
      protocol_metadata_error$df[nrow(protocol_metadata_error$df) + 1,] <-  c(original_filename,
                                                                              filter(warnings, title == "invalid_workbook_version")$message)
      return(version)
    }
  },
  warning = function(w){
    # Track which file triggered the error and the cause (no workbook version column present)
    protocol_metadata_error$df[nrow(protocol_metadata_error$df) + 1,] <-  c(original_filename,
                                                                            filter(warnings, title == "workbook_version_error")$message)
    return("invalid")
  })
}

extractProtocolName <- function(original_filename, protocol_metadata){
  if(is.na(protocol_metadata)) return("invalid")
  
  # Wrapped in an error catcher - if there is no protocol name value present the submission will fail
  tryCatch({
    # Pull protocol name
    # If $protocol_name doesn't exist, will only generate a warning rather than an error
    protocol_metadata$protocol_name
  },
  warning = function(w){
    # Track which file triggered the error and the cause (no protocol name column present)
    protocol_metadata_error$df[nrow(protocol_metadata_error$df) + 1,] <-  c(original_filename,
                                                                            filter(warnings, title == "protocol_name_error")$message)
    return("invalid")
  })
}

checkNumberOfSites <- function(original_filename, sample_metadata){
  if(is.na(sample_metadata)) return("invalid")
  
  # Wrapped in an error catcher - if there is no site column present the submission will fail
  tryCatch({
    if(length(unique(sample_metadata$site_code))==1) {
      return(unique(sample_metadata$site_code))
    } else {
      return("multiple")
    }  
  },
  warning = function(w){
    # Track which file triggered the error and the cause (no site code column present)
    protocol_metadata_error$df[nrow(protocol_metadata_error$df) + 1,] <-  c(original_filename,
                                                                            filter(warnings, title == "missing_site_column")$message)
    return("invalid")
  })
  
}

extractProtocolDate <- function(){
}


