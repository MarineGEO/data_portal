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
    
    extractProtocolSites()
    
    extractProtocolName()
    
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
  })
}

extractWorkbookVersions <- function(original_filename, protocol_metadata){
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

extractProtocolSites <- function(){
}

extractProtocolName <- function(){
}

extractProtocolDate <- function(){
}

# import_status <<- paste("Protocol metadata sheet does not exist for", filenames$original[i], sep=" ")
# 
# showModal(modalDialog(
#   title = "Data Upload Failure",
#   div("Data submissions must use MarineGEO data templates (Excel spreadsheets). The first sheet should be titled 'protocol_metadata'.",
#       "If you are using a protocol metadata sheet, make sure the data entry date values, the name of the protocol, and any site codes in the 'sample metadata' sheet are properly formatted.",
#       "Contact 'marinegeo@si.edu' if you have any questions.",
#       "Please update the following files:", import_status),
#   
#   easyClose = TRUE
# ))
# 
# # Protocol metadata error let's the server app know that no error was created when extracting the information 
# protocol_metadata_error(FALSE)
