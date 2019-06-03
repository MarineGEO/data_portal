# MarineGEO data submission app example 
# This is the server script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu

function(input, output, session) {
  
  # Submission time will store the time a user initially submits data using the humanTime function
  submission_time <- reactiveVal(0)
  
  # Excel sheets with sensitive data must be tracked and stored separately from other files 
  # When users select the sensitive checkbox, this reactive value will change to TRUE
  sensitive <- reactiveVal()
  
  ## Action button logic ###############
  
  ## ... Intro/First page ####################
  # Move to data policy page from introduction
  observeEvent(input$data_policy_intro, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Policy")
  })
  
  # ... Data policy page #####################
  # Accept data policy and move to data submission
  observeEvent( input$new_submission, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Upload")
  })
  
  # Return to welcome screen from data policy 
  # Does not indicate agreement with data policy
  observeEvent(input$return_to_intro, {
    updateTabsetPanel(session, inputId = "nav", selected = "Welcome")
  })
  
  ## ... Data submission page ###############
  # Confirm data submission
  # As long as the user has also provided an excel file and an email address, the initial file will be uploaded to a dropbox directory
  # QA/QC tests will be conducted on the file and a RMD report generated 
  # The user will be moved to a page with the report and whether their submission was successful
  observeEvent(input$submit, {
    # Get the current time
    submission_time(humanTime())
    saveInitialData()
    
    updateTabsetPanel(session, inputId = "nav", selected = "Data Report")
  })
  
  # Return to data policy from submission page
  observeEvent(input$return_to_data_policy, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Policy")
  })
  
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
  
  ## ... Finalize data submission/View report ##################
  # Return to data submission page 
  observeEvent(input$return_to_upload, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Upload")
  })
  
  ## Dropbox functions ##########
  # Once a user provides an email, uploads an excel file, and clicks "submit", 
  # the data is sent to the Dropbox directory.
  saveInitialData <- function() {
    # Upload the Excel file and updated submission log file to Dropbox
    
    # Create a temporary working directory
    tmpdir <- tempdir()
    setwd(tempdir())
    
    # Rename the file to reflect the time the submit button was pressed
    original_file_name <- gsub(".xlsx", "_", input$fileExcel$name)
    filepath_data <- paste0(original_file_name, submission_time(),".xlsx")
    file.rename(input$fileExcel$datapath, filepath_data)

    # Access the submission log from DropBox and append current emails/time/datafile name
    submission_log <- generateSubmissionInfo(filepath_data)
    submission_log_path <- file.path("submission_log.csv")
    write.csv(submission_log, submission_log_path, row.names = FALSE, quote = TRUE)
    
    # upload both the initial data submission to dropbox
    drop_upload(filepath_data, path = "Data")

    # Overwrite the old submission log with the updated info
    drop_upload(submission_log_path, path = "Data", mode = "overwrite")
  }
  
## Helper functions ########
# Called by the saveInitialData() function to acquire the submission log from DB and append new information to it  
generateSubmissionInfo <- function(filepath_data){
  
  # Read in the submission log from Dropbox
  submission_log <- drop_read_csv("Data/submission_log.csv")
  
  # Split emails and create a dataframe based on the number of emails provided
  emails <- strsplit(input$email, split=";")
  num_emails <- length(unlist(emails))
  
  # Crate a new dataframe based on the number of emails provided 
  df <- setNames(data.frame(submission_time(), emails, filepath_data), c("submission_time", "email", "filename"))

  # Append the new data and send back to the dropbox upload function 
  rbind(submission_log, df)
}

  # Prevent users from clicking on tabs in the header 
  # They'll need to use action buttons to move between pages
  shinyjs::disable(selector = '.navbar-nav a')

}