# MarineGEO data submission app 
# This is the server script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu

function(input, output, session) {
  # Functions for extracting submission metadata
  source("extractSubmissionMetadata.R", local=TRUE)
  
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
  # Empty object to hold protocol metadata
  # Similar to filenames object, but one object per protocol-site combination
  output_metadata <- reactiveValues()
  
  # Create object to save errors to 
  QA_results <- reactiveValues(df = setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("test", "filename", "protocol", "sheet_name", "site", "status")),
                               summary = data.frame()) # Summary of all protocols uploaded
  
  # If submission fails QA testing, report status will be sent to False 
  report_status <- reactiveVal()
  
  # Save the filepath of the resulting QA report
  report_path <- reactiveVal()
  
  # If a submission does not have proper protocol metadata fields, submission will automatically fail
  # protocol_metadata_error changes to FALSE if that occurs
  # protocol_metadata_error <- reactiveVal(TRUE)
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

        # Determine how many output CSVs will need to be created 
        # Each protocol-site combination gets a collection of CSV files
        determineOutputs()
        # Run beta QA process 
        testQA()
        # Render the report and save to MarineGEO dropbox
        renderReport()
        # Move user to the data report page
        updateTabsetPanel(session, inputId = "nav", selected = "Data Report")
        # If the QA QC checks were successful, save the curated data in the proper directory
        if(report_status() == "Submission successful" | report_status() == "Some files failed submission process") {
          if(!testing) saveCuratedData()
        }
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
        tags$h4("Synthesis status:", report_status()),
        tags$h4("Contact: MarineGEO (marinegeo@si.edu)"),
        tags$h4("Project affiliation:", paste(project_affiliation$vector, collapse="; ")), tags$br(), 
        
        "This report documents whether the data submission passes MarineGEO's quality assurance/quality control tests. If your submission failed one of the tests, you can view which protocol and sheet failed the test. Please update your data to fix any errors based on this information. If you cannot determine how to interpret a result, modify your data, or believe your data should be able to pass the tests, email MarineGEO (marinegeo@si.edu).",
        tags$br(), tags$br(), 
        
        "Once you've addressed any error(s), resubmit ONLY the data associated with the failed submission. All protocols that pass the QA tests were successfully send to MarineGEO.", tags$br(), tags$br()      
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
        if(!drop_exists(path = paste0("Data/test_initial_directory/", date))){
          
          # If it doesn't, create 
          drop_create(path = paste0("Data/test_initial_directory/", date))
        }
        
        # upload the initial data submission to dropbox
        drop_upload(submission_metadata$new[i],
                    path = paste0("Data/test_initial_directory/", date))
        
      }
      
    } 
    
    # Access the submission log from dropbox and append current emails/time/datafile name
    submission_log <- generateSubmissionInfo()
    
    submission_log_path <- file.path("submission_log.csv")
    write.csv(submission_log, submission_log_path, row.names = FALSE, quote = TRUE)
    
    # Overwrite the old submission log with the updated info
    drop_upload(submission_log_path, path = "Data", mode = "overwrite")
    
  }
  
  saveCuratedData <- function(){
    setwd(tempdir())
    
    for(project in project_affiliation$vector){
      
      # Save each curated protocol to the proper dropbox repository
      for(i in 1:length(submission_data$all_data)){
        
        # Only upload individual file submission if it passed all QA tests
        if(output_metadata$status[i]){
          
          if(!drop_exists(path = paste0("Data/test_curated_directory/",
                                        project, "/",
                                        output_metadata$year[i], "/",
                                        output_metadata$site[i]))){
            
            # If it doesn't, create the site and protocol folders
            drop_create(path = paste0("Data/test_curated_directory/",
                                      project, "/",
                                      output_metadata$year[i], "/",
                                      output_metadata$site[i]))
          }
          
          # Make sure protocol folder exists
          if(!drop_exists(path = paste0("Data/test_curated_directory/",
                                        project, "/",
                                        output_metadata$year[i], "/",
                                        output_metadata$site[i], "/",
                                        output_metadata$protocol[i]))){
            
            drop_create(path = paste0("Data/test_curated_directory/",
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
                      path = paste0("Data/test_curated_directory/",
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
                        path = paste0("Data/test_curated_directory/",
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

# Reads in metadata from each file and generates standard filename
# updateFileNames <- function(){
# 
#   setwd(tempdir())
#   
#   # Wrapped in an error catcher - if user does not upload an excel sheet with a protocol_metadata sheet, the submission will fail
#   tryCatch({
#     # For each file uploaded: 
#     for(i in 1:nrow(input$fileExcel)){
#       # Extract the original file name to be saved in the submission log 
#       filenames$original[i] <- input$fileExcel$name[i]
#       
#       # Import data
#       # Format metadata to be more machine-readable
#       # Turn to wide form, remove first row
#       # Wrapped in an error catcher - if user does not upload an excel sheet with a protocol_metadata sheet, the submission will fail
#       protocol_metadata <- read_xlsx(input$fileExcel$datapath[i], 
#                                      sheet = "protocol_metadata", 
#                                      col_names = c("category", "response"), skip=1) %>%
#         spread(category, response) 
#         
#       # Read in sample metadata to get site code
#       sample_metadata <- read_xlsx(input$fileExcel$datapath[i], sheet = "sample_metadata")
#       
#       # Format the date properly
#       # Version 0.3 has single date value
#       if(protocol_metadata$workbook_version == "v0.3.0"){
#         protocol_metadata <- mutate(protocol_metadata, data_entry_date = as.Date(as.numeric(data_entry_date), origin = "1899-12-30"))
#         filenames$year[i] <- year(protocol_metadata$data_entry_date)
#         filenames$data_entry_date[i] <- as.character(protocol_metadata$data_entry_date)
#       # Version 0.4 has a unique cell each for day, month and year
#       } else {
#         filenames$year[i] <- protocol_metadata$data_entry_year
#         filenames$data_entry_date[i] <- paste(protocol_metadata$data_entry_year, 
#                                                protocol_metadata$data_entry_month, 
#                                                protocol_metadata$data_entry_day, sep="-")
#       }
#       
#       filenames$wb_version[i] <- protocol_metadata$workbook_version
#       filenames$protocol[i] <- protocol_metadata$protocol_name
#       
#       # temporary measure: document site as "multiple" if more than one site in a protocol
#       # filenames$all_sites represents a vector will all unique site codes, which will help create the necessary number of output CSVs later
#       if(length(unique(sample_metadata$site_code))==1) {
#         filenames$site[i] <- unique(sample_metadata$site_code)
#         filenames$all_sites[i] <- unique(sample_metadata$site_code)
#       } else {
#         filenames$site[i] <- "multiple"
#         filenames$all_sites[i] <- list(unique(sample_metadata$site_code))
#       }
#       
#       # file name will be [Protocol]_[MarineGEO site code]_[data entry date in YYYY-MM-DD format]
#       filenames$new[i] <- paste0(filenames$protocol[i], "_",
#                                  filenames$site[i], "_",
#                                  filenames$data_entry_date[i], ".xlsx")
# 
#       file.rename(input$fileExcel$datapath[i], filenames$new[i])
#       
#     }
#     
#   },
#   error = function(e){
#     import_status <<- paste("Protocol metadata sheet does not exist for", filenames$original[i], sep=" ")
# 
#     showModal(modalDialog(
#       title = "Data Upload Failure",
#       div("Data submissions must use MarineGEO data templates (Excel spreadsheets). The first sheet should be titled 'protocol_metadata'.",
#           "If you are using a protocol metadata sheet, make sure the data entry date values, the name of the protocol, and any site codes in the 'sample metadata' sheet are properly formatted.",
#           "Contact 'marinegeo@si.edu' if you have any questions.",
#           "Please update the following files:", import_status),
# 
#       easyClose = TRUE
#     ))
# 
#     protocol_metadata_error(FALSE)
# 
#   })
#   
# }
  
# Called by the saveInitialData() function to acquire the submission log from DB and append new information to it  
generateSubmissionInfo <- function(){
  
  # Read in the submission log from Dropbox
  submission_log <- drop_read_csv("Data/submission_log.csv")

  # Collapse all variables to fit into a row in the submission log. 
  # They can be split by ; later 
  protocols <- paste(unique(submission_metadata$protocol), collapse = "; ")
  original_filenames <- paste(submission_metadata$original, collapse = "; ")  
  standardized_filenames <- paste(submission_metadata$new, collapse="; ")
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
  # A. Get protocol name
  # B. determine which has multiple sites
  # C. Create list of output protocols
  index <- 1

  for(i in 1:length(submission_metadata$new)){
    if(submission_metadata$site[i] != "multiple"){
      output_metadata$protocol[[index]] <- submission_metadata$protocol[i] 
      output_metadata$filename_new[[index]] <- submission_metadata$new[i]
      output_metadata$filename_original[[index]] <- submission_metadata$original[i]
      output_metadata$year[[index]] <- submission_metadata$year[i]
      output_metadata$site[[index]] <- submission_metadata$site[i]
      output_metadata$data_entry_date[[index]] <- submission_metadata$data_entry_date[i]
      output_metadata$protocol_df[[index]] <- submission_metadata$protocol_df[i][[1]]
      index <- index + 1
      
    } else {
      for(j in 1:length(unlist(submission_metadata$all_sites[i]))){
        output_metadata$protocol[[index]] <- submission_metadata$protocol[i] 
        output_metadata$filename_new[[index]] <- submission_metadata$new[i]
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
  for(i in 1:length(output_metadata$filename_new)){

    current_protocol <- output_metadata$protocol[i]
    
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
      
      # Keep only site information 
      if("site_code" %in% colnames(protocol_df[[sheet_name]])){
        protocol_df[[sheet_name]] <- protocol_df[[sheet_name]] %>%
          filter(site_code == output_metadata$site[i])
      }
      
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
    } else output_metadata$status[i] <- FALSE
  
  }
  
  QA_results$summary <- QA_results$df %>%
    group_by(filename, site) %>%
    summarize(protocol = first(protocol), 
              result = ifelse(any(status != "Passed"), "Submission failed", "Submission passed"))
  
}

# Generate the QA report in markdown
renderReport <- function(){
  
  setwd(original_wd)
  
  # Check if the submission failed any QA tests
  if(all(QA_results$summary$result == "Submission passed")){
    report_status("Submission successful")
  } else if(all(QA_results$summary$result == "Submission failed")){
    report_status("Submission failed")
  } else{
    report_status("Some files failed submission process")
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
                path = paste0("Data/submission_reports"))
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