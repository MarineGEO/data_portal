# MarineGEO data submission app 
# This is the server script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu

function(input, output, session) {
  # Functions for extracting submission metadata
  source("extractSubmissionMetadata.R", local=TRUE)
  # QA functions
  #source("./qaqc_scripts/qaqcTests.R", local=TRUE)
  source("./qaqc_scripts/id_relationship_test.R", local=TRUE)
  source("./qaqc_scripts/min_max_test.R", local=TRUE)
  source("./qaqc_scripts/numeric_type_test.R", local=TRUE)
  source("./qaqc_scripts/sample_metadata_test.R", local=TRUE)
  source("./qaqc_scripts/taxa_id_relationship_test.R", local=TRUE)
  
  # Submission time will store the time a user initially submits data using the humanTime function
  submission_time <- reactiveVal(0)
  
  # Excel sheets with sensitive data must be tracked and stored separately from other files 
  # When users select the sensitive checkbox, this reactive value will change to TRUE
  sensitive <- reactiveVal()
  
  # Create empty list to hold original and standardized filenames of uploaded protocols + related metadata
  filenames <- reactiveValues()
  submission_metadata <- reactiveValues()
  
  # Project affiliation of submission
  project_affiliation <- reactiveValues(vector = c())
  
  # Create empty list to hold protocol data for QA testing and curation
  submission_data <- reactiveValues(all_data = list())
  
  # Empty object to hold output protocol data and metadata
  # Output is one df per protocol-sheet-site-collection year combination
  output_data <- reactiveValues()
  output_metadata <- reactiveValues(df = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("filename",
                                                                                            "protocol",
                                                                                            "data_entry_date",
                                                                                            "site_code",
                                                                                            "year_collected")))
  output_directories <- reactiveValues(sites = list(),
                                       years = list(),
                                       protocols = list())
  
  # Create object to save errors to 
  QA_results <- reactiveValues(df = setNames(data.frame(matrix(ncol = 7, nrow = 0)), 
                                             c("test", "filename", "protocol", "sheet_name", "column_name", "row_numbers", "values"))) 
  
  # Objects hold each protocol and related metadata as they undergo QA in called functions
  current_protocol <- reactiveVal()
  protocol_sheets <- reactiveVal()
  original_filename_qa <- reactiveVal()
  stored_protocol <- reactiveValues()
  
  # If submission fails QA testing, report status will be sent to False 
  report_status <- reactiveVal()
  
  # Save the filepath of the resulting QA report
  report_path <- reactiveVal()
  
  # If a submission does not have proper protocol metadata fields, submission will automatically fail
  # protocol metadata error is a table that tracks the errors and associated files
  protocol_metadata_error <- reactiveValues(df = setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("filename", "error")))
  
  # Store specific error message(s) if protocol metadata information extraction fails 
  protocol_error_message <- reactiveVal()
  
  ## Welcome and Data Policy action button logic ###############
  
  ## ... Intro/First page
  # Move to data policy page from introduction
  observeEvent(input$data_policy_intro, {
    if(!testing){
      updateTabsetPanel(session, inputId = "nav", selected = "Data Policy")
    } else updateTabsetPanel(session, inputId = "nav", selected = "Data Upload")
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
  
  # Data policy table
  output$data_policy <- renderTable({data_policy_table})
  
  ##  Data submission page button logic and observers ##############
  
  # Return to data policy from submission page
  observeEvent(input$return_to_data_policy, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Policy")
  })
  
  ## ... submit data button - file save, QA, report display functions ##############
  # As long as the user has also provided an excel file and an email address, the initial file will be uploaded to a dropbox directory
  # QA/QC tests will be conducted on the file and a RMD report generated 
  # The user will be moved to a page with the report and whether their submission was successful
  observeEvent(input$submit, {
    
    # Get the current time
    submission_time(humanTime())
    
    # Create inital submission receipt (save email and submission time)
    if(!testing) initialReceipt() 
    
    # Make sure each file is an excel (.xlsx) file
    # And data entry date - protocol - site information can all be extracted
    file_test <- checkFileExtensions()
    
    # Only upload data if all files are .xlsx
    if(file_test){
      showModal(modalDialog(
        title = "Data Uploading", 
        div("Thank you for submitting data to MarineGEO! Your data is currently undergoing QA tests, 
            and being saved to our database. This will take a few moments. If you are uploading data for multiple sites or more than three protocols, it could take from one to three minutes. Once complete, this page will update, and you will have access to a report 
            with additional details about the status of your submission."),
        
        easyClose = TRUE
      ))
      
      # Save filnames and create standardized filenames based on protocol-site-data entry date
      # updateFileNames()
      extractProtocolMetadata()
      
      # Upload initial files to dropbox and run QA checks
      if(!testing) saveInitialData()
      
      # If no errors are recorded continue with the submission 
      if(nrow(protocol_metadata_error$df)==0){

        # Run beta QA process 
        QAQC()
        # Determine how many output CSVs will need to be created 
        # Each protocol-site-collection year combination gets a collection of CSV files
        determineOutputs()
        # Render the report and save to MarineGEO dropbox
        renderReport()
        # Move user to the data report page
        updateTabsetPanel(session, inputId = "nav", selected = "Data Report")
        # If the QA QC checks were successful, save the curated data in the proper directory
        # if(report_status() == "Submission successful" | report_status() == "Some files failed submission process") {
          if(!testing) saveCuratedData()
        # }
      } else {
        # If any elements of the protocol metadata failed
        showModal(modalDialog(
          title = "Failed Submission", 
          size = "l",
          div("The application failed to obtain the necessary metadata from one or more of your uploaded files.",
              "The following table lists which files produced an error. Contact 'marinegeo@si.edu' if you need further assistance.",
              tags$br(), 
              renderTable(protocol_metadata_error$df)),
          
          easyClose = TRUE
        ))
      }
      
    } else {
      # This resets the file input but doesn't erase the file path, thus triggering errors even if the user then uploads the correct file type
      shinyjs::reset("fileExcel")
      
      showModal(modalDialog(
        title = "Failed Submission", 
        div(protocol_error_message()),
        
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
    if(!testing){
      if(input$email != "" & 
         grepl("@", input$email) &
         !is.null(input$fileExcel) &
         input$sensitive_prompt != "Not specified"){
        shinyjs::enable("submit")
      } else {
        shinyjs::disable("submit")
      }
    } else shinyjs::enable("submit")
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
  
  output$html_report_intro <- renderUI({
    if(file.exists(paste0("./www/", report_path()))){
      
      div(
        tags$h4("Report date: ", submission_time()),
        # tags$h4("Synthesis status:", report_status()),
        tags$h4("Contact: MarineGEO (marinegeo@si.edu)"),
        tags$h4("Project affiliation:", paste(project_affiliation$vector, collapse="; ")), tags$br(), 
        
        "This report documents whether the data submission passes MarineGEO's quality assurance/quality control tests. If your submission failed one of the tests, you can view which protocol and sheet failed the test. Please update your data to fix any issues based on this information. If you cannot determine how to interpret a result, modify your data, or believe your data should be able to pass the tests, email MarineGEO (marinegeo@si.edu).",
        tags$br(), tags$br(), 
        
        "Once you've addressed any issues(s), resubmit ONLY the file that did not pass the test", tags$br(), tags$br()
        
        )
  
    }
  })
    
  output$html_report_qa <- renderUI({
    if(file.exists(paste0("./www/", report_path()))){
      
      div(        
        tags$h4("QA/QC Test Results"),
        "Test numeric variables: All data associated with numeric-type columns should either be a numeric value or 'NA'. If a given sheet fails this test, then a column within that sheet has a character value. Ensure all 'NA' values are uppercase.",
        tags$br(), tags$br()
      )
    }
  })
  
  output$qa_table_results <- renderDataTable({
    QA_results$df
  })

  output$qa_summary_results <- renderDataTable({
    QA_results$summary
  })
  
  observeEvent(input$return_to_upload, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Upload")
  })
  
  ## ... Download Report ##############
  # Note: It appears downloadHandler changes the wd to a temporary dir 
  # Have to set wd back to original in order to clear the HTML report when the app closes
  output$downloadReport <- downloadHandler(
    filename = function() {
      report_path()
    },
    content = function(file) {
      # As far as I can tell, you can't return a file path here, 
      # this function moves a given file to its required directory
      file.copy(paste0(original_wd, "/www/", report_path()), file)
      
    }, 
    contentType = "text/html"
  )
  
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
    #tmpdir <- tempdir()
    setwd(tempdir())
    date <- as.character(submissionDate())
    
    if(nrow(protocol_metadata_error$df)==0){
      # For each file uploaded:
      for(i in 1:nrow(input$fileExcel)){
        
        # Make sure date folder exists
        if(!drop_exists(path = paste0("MarineGEO/Data/test_initial_directory/", date))){
          
          # If it doesn't, create 
          drop_create(path = paste0("MarineGEO/Data/test_initial_directory/", date))
        }
        
        # upload the initial data submission to dropbox
        drop_upload(submission_metadata$new_filename[i],
                    path = paste0("MarineGEO/Data/test_initial_directory/", date))
        
      }
      
    } 
    
    # Access the submission log from dropbox and append current emails/time/datafile name
    submission_log <- generateSubmissionInfo()
    
    submission_log_path <- file.path("submission_log.csv")
    write.csv(submission_log, submission_log_path, row.names = FALSE, quote = TRUE)
    
    # Overwrite the old submission log with the updated info
    drop_upload(submission_log_path, path = "MarineGEO/Data/", mode = "overwrite")
    
  }
  
  saveCuratedData <- function(){
    setwd(tempdir())

    for(project in project_affiliation$vector){
      
      # Save each curated protocol to the proper dropbox repository
      for(i in 1:length(submission_data$all_data)){
        
        # Only upload individual file submission if it passed all QA tests
        if(output_metadata$status[i]){
          

          if(!drop_exists(path = paste0("MarineGEO/Data/test_curated_directory/",
                                        project, "/",
                                        output_metadata$year[i], "/",
                                        output_metadata$site[i]))){
            
            # If it doesn't, create the site and protocol folders
            drop_create(path = paste0("MarineGEO/Data/test_curated_directory/",
                                      project, "/",
                                      output_metadata$year[i], "/",
                                      output_metadata$site[i]))
          }
          
          # Make sure protocol folder exists
          if(!drop_exists(path = paste0("MarineGEO/Data/test_curated_directory/",
                                        project, "/",
                                        output_metadata$year[i], "/",
                                        output_metadata$site[i], "/",
                                        output_metadata$protocol[i]))){
            
            drop_create(path = paste0("MarineGEO/Data/test_curated_directory/",
                                      project, "/",
                                      output_metadata$year[i], "/",
                                      output_metadata$site[i], "/",
                                      output_metadata$protocol[i]))
            
          }
          # Write protocol metadata sheet
          write_csv(output_metadata$protocol_df[[i]], 
                    paste0(output_metadata$protocol[i], "_",
                           output_metadata$site[i], "_",
                           output_metadata$data_entry_date[i], "_", 
                           "protocol_metadata", ".csv"))
          
          # file name will be [Protocol]_[MarineGEO site code]_[data entry date in YYYY-MM-DD format]_[sheet]
          drop_upload(paste0(output_metadata$protocol[i], "_",
                             output_metadata$site[i], "_",
                             output_metadata$data_entry_date[i], "_", 
                             "protocol_metadata", ".csv"),
                      path = paste0("MarineGEO/Data/test_curated_directory/",
                                    project, "/",
                                    output_metadata$year[i], "/",
                                    output_metadata$site[i], "/",
                                    output_metadata$protocol[i]))
          
          for(sheet in names(submission_data$all_data[[i]])) {

            # Write the curated set to the temporary directory and send to Dropbox
            write_csv(submission_data$all_data[[i]][[sheet]], 
                      paste0(output_metadata$protocol[i], "_",
                             output_metadata$site[i], "_",
                             output_metadata$data_entry_date[i], "_", 
                             sheet, ".csv"))
            
            # file name will be [Protocol]_[MarineGEO site code]_[data entry date in YYYY-MM-DD format]_[sheet]
            drop_upload(paste0(output_metadata$protocol[i], "_",
                               output_metadata$site[i], "_",
                               output_metadata$data_entry_date[i], "_", 
                               sheet, ".csv"),
                        path = paste0("MarineGEO/Data/test_curated_directory/",
                                      project, "/",
                                      output_metadata$year[i], "/",
                                      output_metadata$site[i], "/",
                                      output_metadata$protocol[i]))
          }
        }
      }
    }
    setwd(original_wd)
  }
  
## Helper functions ##########
# Records any submission attempt
# Allows MarineGEO to know someone is trying to submit data in case they are getting errors that crash the application
initialReceipt <- function(){
  setwd(tempdir())
  
  writeLines(input$email, paste0(submission_time(), ".txt"))
  drop_upload(paste0(submission_time(), ".txt"), 
                     path = "MarineGEO/Data/submission_reports/initial_submission_receipts", mode = "overwrite")
  
  setwd(original_wd)
}  
  
# Called by the saveInitialData() function to acquire the submission log from DB and append new information to it  
generateSubmissionInfo <- function(){
  
  # Read in the submission log from Dropbox
  submission_log <- drop_read_csv("MarineGEO/Data/submission_log.csv")

  # Collapse all variables to fit into a row in the submission log. 
  # They can be split by ; later 
  protocols <- paste(unique(submission_metadata$protocol), collapse = "; ")
  original_filenames <- paste(submission_metadata$original_filename, collapse = "; ")  
  standardized_filenames <- paste(submission_metadata$new_filename, collapse="; ")
  emails <- tolower(input$email)
  wb_versions <- paste(submission_metadata$wb_version, collapse = "; ")  
  
  # check which projects the emails provided with the submission are affiliated with
  project <- unique(filter(roster, email %in% emails)$project_affiliation)
  
  # If they're not affiliated, the curated submission will be sent to the unaffiliated submissions directory
  if(length(project)==0){
    project_affiliation$vector <- "unaffiliated_submissions"
  } else {
    project_affiliation$vector <- project
  }
  
  # Crate a new dataframe based on the number of emails provided 
  df <- setNames(data.frame(submission_time(), input$email, protocols, standardized_filenames, original_filenames, paste(project_affiliation$vector, collapse="; "),
                            portal_version, wb_versions, NA),
                 c("submission_time", "email", "protocols", "standardized_filenames", "original_filenames", "project", 
                   "portal_version", "workbook_version", "notes"))

  # Append the new data and send back to the dropbox upload function 
  rbind(submission_log, df)
}

# Test each file extension and ensure it's an xslx file
checkFileExtensions <- function(){

  if(any(!grepl("xlsx", input$fileExcel$name))){
    # Update protocol error message with proper warning
    protocol_error_message(filter(warnings, title == "no_excel_files")$message)
    return(FALSE)
    
  } else return(TRUE)

}

determineOutputs <- function(){
  # Cycle through each uploaded file to:
  # A. determine which has multiple sites
  # B. Determine which has multiple collection years
  # C. Create list of output filenames
  directory_sites <- c()
  directory_years <- c()
  directory_protocols <- c()
  
  for(i in 1:length(submission_data$all_data)){
    print(submission_metadata$protocol[i]) # prints protocol name
    #print(names(submission_data$all_data[[i]])) # prints vector of sheet names
    #print(submission_data$all_data[[i]]) # prints dataframe, NULL if no data
    
    # Get current protocol name and data entry date
    protocol <- submission_metadata$protocol[i]
    data_entry_date <- submission_metadata$data_entry_date[i]
    
    sample_metadata <- submission_data$all_data[[i]]$sample_metadata
    # unique sites in the sample metadata file
    sites <- unique(sample_metadata$site_code)
    # unique data collection years in the sample metadata file
    years <- unique(mutate(sample_metadata, 
                           year_collected = year(anydate(sample_collection_date)))$year_collected)
    
    for(sheet in names(submission_data$all_data[[i]])){
      # Sheets with no data are recorded as NULL in submission_data$all_data
      if(!is.null(submission_data$all_data[[i]][sheet])){
        
        if(length(sites) > 1 | length(years) > 1){
          
          
        } else {
          # Create new filename
          new_filename <- paste(gsub("_", "-", protocol), 
                                sites, data_entry_date, sheet, sep="_")
          
          # Save the file and metadata
          output_data[[new_filename]] <- submission_data$all_data[[i]][sheet]
          
          output_metadata$df <- output_metadata$df %>%
            bind_rows(setNames(as.data.frame(t(c(new_filename, protocol, data_entry_date, sites, years))), 
                               c("filename","protocol","data_entry_date","site_code","year_collected"))) %>%
            mutate_all(as.character)
          
        } 
      }
    }
    
    directory_sites <- c(directory_sites, sites)
    directory_years <- c(directory_years, years)
    directory_protocols <- c(directory_protocols, protocol)
  }
  
  print(output_metadata$df)
  
  output_directories$sites <- unique(directory_sites)
  output_directories$years <- unique(directory_years)
  output_directories$protocols <- unique(directory_protocols)
}

QAQC <- function(){
  # Data will be read in, tested, and stored in submission_data$all_data
  # Submission_data$all_data is a list of lists, where each first order list is a sheet in a protocol
  # And each second order list represents a protocol
  
  # Loop through each uploaded protocol
  for(i in 1:length(submission_metadata$original_filename)){
    
    original_filename_qa(submission_metadata$original_filename[i])
    current_protocol(submission_metadata$protocol[i])
    
    # Get names of sheets for given protocol
    protocol_sheets(protocol_structure %>%
      filter(protocol == current_protocol()) %$% # Note use of %$% rather than %>%, allows you to use $ in unique and get results as a vector
      unique(.$sheet))
    
    # Create an empty list, each object will be a sheet for the protocol
    protocol_df <- vector("list", length(protocol_sheets()))
    names(protocol_df) <- protocol_sheets()
    
    # Read in each sheet for the protocol, assign to respective list object 
    for(sheet_name in protocol_sheets()) {
      df <- read_excel(submission_metadata$new_filename[i], 
                       sheet = sheet_name, 
                       na = c("NA", "This cell will autocalculate", "N/A"))
      
      # Need to prevent empty sheets from getting uploaded
      # For a sheet with no rows, no data frame will be associated at that branch of the list and the sheet name won't be in the testing list
      # TO DO - Add entry to QA_results_table in else statement
      if(nrow(df) > 0){
        protocol_df[[sheet_name]] <- df
      } else{
        protocol_sheets_non_reactive <- protocol_sheets()
        protocol_sheets(protocol_sheets_non_reactive[protocol_sheets_non_reactive != sheet_name])
      }
    }
    
    # Save protocol data to a reactive list object so QA tests can access within environment
    stored_protocol$df <- protocol_df
    
    # Run  QA tests
    # Each function is in its own script
    QA_results$df <- QA_results$df %>%
      bind_rows(checkSampleMetadata()) %>%
      bind_rows(checkIDRelationships()) %>%
      bind_rows(checkTaxaRelationships()) %>%
      bind_rows(testNumericType()) %>%
      bind_rows(numericMinMaxTest())
    
    # Add the protocol to the overall submission data list 
    submission_data$all_data[[paste(current_protocol(), 
                                    submission_metadata$original_filename[i], 
                                    submission_metadata$site[i], sep="_")]] <- protocol_df
    
  }
  
}


# Generate the QA report in markdown
renderReport <- function(){
  
  setwd(original_wd)
  
  # Check if the submission failed any QA tests
  if(nrow(QA_results$df) == 0){
    report_status("Submission successful")
  } else{
    report_status("Some files did not pass tests")
  }

  ## Write RMarkdown report #########

  rmarkdown::render(input = "./marinegeo_submission_report.Rmd",
                    output_format = "html_document",
                    output_file = paste0("submission_report_", submission_time(), ".html"),
                    output_dir = "./www/")
  
  report_path(paste0("submission_report_", submission_time(), ".html"))
  
  
  if(!testing){
    # Send the report to the dropbox
    drop_upload(paste0("./www/", report_path()),
                path = paste0("MarineGEO/Data/submission_reports"))
  }
  
}

## Clear the RMarkdown report when the session ends #####
session$onSessionEnded(function() {
  observe({
    setwd(original_wd)
    if(!is.null(report_path())){
      system(paste("rm -f", paste0("./www/", report_path())))
    }
  })
})

# Prevent users from clicking on tabs in the header 
# They'll need to use action buttons to move between pages
shinyjs::disable(selector = '.navbar-nav a')

}