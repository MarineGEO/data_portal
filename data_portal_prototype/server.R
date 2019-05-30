# MarineGEO data submission app example 
# This is the server script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu

function(input, output, session) {
  
  # Submission time will store the time a user initially submits data using the humanTime function
  submission_time <- reactiveVal(0)
  
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
  
  # Prevent the "submit" button on the data submission page you be pressed if a user does not provide an email address. 
  observe({
    if(input$email != "" & !is.null(input$fileExcel)){
      shinyjs::enable("submit")
    } else {
      shinyjs::disable("submit")
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
    # Upload the Excel file to Dropbox
    
    # Create a temporary working directory
    tmpdir <- tempdir()
    setwd(tempdir())
    
    # Rename the file to reflect the time the submit button was pressed
    filePath_data <- paste0(submission_time(),".xlsx")
                            
    file.rename(input$fileExcel$datapath, filePath_data)
    
    # Take the email information and save it as a csv
    initial_upload_metadata <- sprintf("%s_submission_metadata.csv", humanTime())
    # # Write the data to a temporary file locally
    #filePath <- file.path(tempdir(), initial_upload_metadata)
    filePath <- file.path(initial_upload_metadata)
    write.csv(input$email, filePath, row.names = FALSE, quote = TRUE)
    
    # upload both the initial data and the metadata to dropbox
    drop_upload(filePath_data, path = "Data")
    drop_upload(filePath, path = "Data")
  }
  
  # Prevent users from clicking on tabs in the header 
  # They'll need to use action buttons to move between pages
  shinyjs::disable(selector = '.navbar-nav a')

}