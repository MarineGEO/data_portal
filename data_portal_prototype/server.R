# MarineGEO data submission app example 
# This is the server script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu

function(input, output, session) {
  
  # Submission time will store the time a user initially submits data using the humanTime function
  submission_time <- reactiveVal(0)
  
  # Excel sheets with sensitive data must be tracked and stored separately from other files 
  # When users select the sensitive checkbox, this reactive value will change to TRUE
  sensitive <- reactiveVal()
  
  ## Welcome and Data Policy action button logic ###############
  
  ## ... Intro/First page
  # Move to data policy page from introduction
  observeEvent(input$data_policy_intro, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Policy")
  })
  
  # ... Data policy page
  # Accept data policy and move to data submission
  observeEvent( input$new_submission, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Upload")
  })
  
  # Return to welcome screen from data policy 
  # Does not indicate agreement with data policy
  observeEvent(input$return_to_intro, {
    updateTabsetPanel(session, inputId = "nav", selected = "Welcome")
  })
  
  ##  Data submission page button logic and observers ##############
  
  # Return to data policy from submission page
  observeEvent(input$return_to_data_policy, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Policy")
  })
  
  ## ... submit data button ##############
  # As long as the user has also provided an excel file and an email address, the initial file will be uploaded to a dropbox directory
  # QA/QC tests will be conducted on the file and a RMD report generated 
  # The user will be moved to a page with the report and whether their submission was successful
  observeEvent(input$submit, {
    
    # Get the current time
    submission_time(humanTime())
    
    # Make sure each file is an excel (.xlsx) file
    file_test <- checkFileExtensions()
    
    # Only upload data if all files are .xlsx
    if(file_test){
       saveInitialData()
       updateTabsetPanel(session, inputId = "nav", selected = "Data Report")
    } else {
      # This resets the file input but doesn't erase the file path, thus triggering errors even if the user then uploads the correct file type
      shinyjs::reset("fileExcel")
      
      showModal(modalDialog(
        title = "Invalid file type", 
        div("One or more files you uploaded are not Microsoft Excel files. Please reload the application and ensure all uploads have '.xlsx' file extensions."),
        
        easyClose = TRUE
      ))
    }
    
  })
  
  ## ... data submission button lock conditions ###########
  # Prevent the "submit" button on the data submission page to be pressed if a user does not 
  # A. provide an email address. 
  # B. select if their data does or does not contain sensitive information 
  # C. upload an excel document 
  observe({
    if(input$email != "" & 
       grepl("@", input$email) &
       !is.null(input$fileExcel) &
       input$sensitive_prompt != "Not specified"){
      shinyjs::enable("submit")
    } else {
      shinyjs::disable("submit")
    }
  })
  
  observeEvent(input$sensitive_prompt, {
    if(input$sensitive_prompt == "Yes, my data contains sensitive information") {
      sensitive(TRUE)
    } else {
      sensitive(FALSE)
    }
  }) 
  
  ## Finalize data submission/View report ##################
  # Return to data submission page 
  observeEvent(input$return_to_upload, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Upload")
  })
  
  ## Dropbox functions ####################
  # There can be up to 5 instances of writing data to Dropbox per data submission session
  # 1. initial data (raw excel spreadsheets)
  # 2. initial submission-related metadata
  # 3. curated data (flat csv files)
  # 4. RMD report of QA/QC 
  # 5. curated data merged into public-facing directories (flat csv files)
  
  # Once a user provides an email, uploads an excel file, and clicks "submit", 
  # the data is sent to the Dropbox directory.
  saveInitialData <- function() {
    # Upload the Excel file and updated submission log file to Dropbox
    # Create a temporary working directory
    tmpdir <- tempdir()
    setwd(tempdir())
    
    new_filepaths <- c()
    # For each file uploaded: 
    for(i in 1:nrow(input$fileExcel)){
      # Rename the file to reflect the time the submit button was pressed
      original_file_name <- gsub(".xlsx", "_", input$fileExcel$name[i])
      filepath_data <- paste0(original_file_name, submission_time(),".xlsx")
      file.rename(input$fileExcel$datapath[i], filepath_data)
      
      # Collect a vector with all of the new filepath names
      new_filepaths <- append(new_filepaths, filepath_data)

      # upload the initial data submission to dropbox
      drop_upload(filepath_data, path = "Data")
    }
    
    # Access the submission log from dropbox and append current emails/time/datafile name
    submission_log <- generateSubmissionInfo(new_filepaths)
    
    submission_log_path <- file.path("submission_log.csv")
    write.csv(submission_log, submission_log_path, row.names = FALSE, quote = TRUE)
    
    # Overwrite the old submission log with the updated info
    drop_upload(submission_log_path, path = "Data", mode = "overwrite")
    
  }
  
## Helper functions ##########

# Called by the saveInitialData() function to acquire the submission log from DB and append new information to it  
generateSubmissionInfo <- function(new_filepaths){
  
  # Read in the submission log from Dropbox
  submission_log <- drop_read_csv("Data/submission_log.csv")
  
  # Split emails and create a dataframe based on the number of emails provided
  emails <- strsplit(input$email, split=";")
  num_emails <- length(unlist(emails))
  
  filepaths <- paste(new_filepaths, collapse=",")
  
  # Crate a new dataframe based on the number of emails provided 
  df <- setNames(data.frame(submission_time(), emails, filepaths), c("submission_time", "email", "filenames"))

  # Append the new data and send back to the dropbox upload function 
  rbind(submission_log, df)
}

# Test each file extension and ensure it's an xslx file
checkFileExtensions <- function(){

  if(any(!grepl("xlsx", input$fileExcel$name))){
    return(FALSE)
  } else return(TRUE)

}

  # Prevent users from clicking on tabs in the header 
  # They'll need to use action buttons to move between pages
  shinyjs::disable(selector = '.navbar-nav a')

}