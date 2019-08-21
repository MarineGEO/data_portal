# MarineGEO data submission app example 
# This is the server script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu

function(input, output, session) {
  
  # Submission time will store the time a user initially submits data using the humanTime function
  submission_time <- reactiveVal(0)
  
  # Excel sheets with sensitive data must be tracked and stored separately from other files 
  # When users select the sensitive checkbox, this reactive value will change to TRUE
  sensitive <- reactiveVal()
  
  # Create empty list to hold original and standardized filenames of uploaded protocols
  filenames <- reactiveValues()
  
  # Project affiliation of submission
  project_affiliation <- reactiveValues(vector = c())
  
  # Create empty list to hold protocol data for QA testing and curation
  submission_data <- reactiveValues(all_data = list())
  
  # Create object to save errors to 
  QA_results <- reactiveValues(df = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("test", "filename", "protocol", "sheet_name", "status")))
  
  # If submission fails QA testing, report status will be sent to False 
  report_status <- reactiveVal()
  
  # Save the filepath of the resulting QA report
  report_path <- reactiveVal()
  
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
  
  ## ... submit data button - file save, QA, report display functions ##############
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
       # Upload initial files to dropbox and run QA checks
       saveInitialData()
       testQA()
       # Render the report and save to MarineGEO dropbox
       renderReport()
       # Move user to the data report page
       updateTabsetPanel(session, inputId = "nav", selected = "Data Report")
       # If the QA QC checks were successful, save the curated data in the proper directory
       if(report_status()) {saveCuratedData()}
       
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
  
  output$html_report <- renderUI({
    if(file.exists(paste0("./www/", report_path()))){
      
      if(report_status()) {
        status <- "Successful"
      } else status <- "Failed"
      
      # tags$iframe(style="height:600px; width:100%",
      #             src=report_path())
      
      div(
        "Report date: ", submission_time(), tags$br(),
        "Synthesis status:", status, tags$br(),
        "Contact: MarineGEO (marinegeo@si.edu)",
        
        tags$br(), tags$br(), 
        
        "QA/QC Test Results", tags$br()
      )
      
    }
  })
    
  
  output$qa_table_results <- renderDataTable({
    QA_results$df
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
    
    # Save filnames and create standardized filenames based on protocol-site-data entry date
    updateFileNames()
    
    # For each file uploaded:
    for(i in 1:nrow(input$fileExcel)){
      
      # Make sure site folder exists
      if(!drop_exists(path = paste0("Data/test_initial_directory/",
                                    filenames$year[i], "/", 
                                    filenames$site[i]))){
        
        # If it doesn't, create the site and protocol folders 
        drop_create(path = paste0("Data/test_initial_directory/",
                                  filenames$year[i], "/", 
                                  filenames$site[i])) 
      }
        
      # Make sure protocol folder exists
      if(!drop_exists(path = paste0("Data/test_initial_directory/",
                                    filenames$year[i], "/", 
                                    filenames$site[i], "/",
                                    filenames$protocol[i]))){
        
        drop_create(path = paste0("Data/test_initial_directory/",
                                  filenames$year[i], "/", 
                                  filenames$site[i], "/",
                                  filenames$protocol[i]))
      }
      
      # upload the initial data submission to dropbox
      drop_upload(filenames$new[i],
                  path = paste0("Data/test_initial_directory/",
                                filenames$year[i], "/",
                                filenames$site[i], "/",
                                filenames$protocol[i]))
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
        
        if(!drop_exists(path = paste0("Data/test_curated_directory/",
                                      project, "/",
                                      filenames$year[i], "/",
                                      filenames$site[i]))){
          
          # If it doesn't, create the site and protocol folders
          drop_create(path = paste0("Data/test_curated_directory/",
                                    project, "/",
                                    filenames$year[i], "/",
                                    filenames$site[i]))
        }
        
        # Make sure protocol folder exists
        if(!drop_exists(path = paste0("Data/test_curated_directory/",
                                      project, "/",
                                      filenames$year[i], "/",
                                      filenames$site[i], "/",
                                      filenames$protocol[i]))){
          
          drop_create(path = paste0("Data/test_curated_directory/",
                                    project, "/",
                                    filenames$year[i], "/",
                                    filenames$site[i], "/",
                                    filenames$protocol[i]))
          
        }
        
        for(sheet in names(submission_data$all_data[[i]])) {
          # Make sure sheet folder exists
          if(!drop_exists(path = paste0("Data/test_curated_directory/",
                                        project, "/",
                                        filenames$year[i], "/",
                                        filenames$site[i], "/",
                                        filenames$protocol[i], "/",
                                        sheet))){
            
            drop_create(path = paste0("Data/test_curated_directory/",
                                      project, "/",
                                      filenames$year[i], "/",
                                      filenames$site[i], "/",
                                      filenames$protocol[i], "/",
                                      sheet))
            
          }
          # Write the curated set to the temporary directory and send to Dropbox
          write_csv(submission_data$all_data[[i]][[sheet]], 
                    paste0(filenames$protocol[i], "_",
                           filenames$site[i], "_",
                           filenames$data_entry_date[i], "_", 
                           sheet, ".csv"))
          
          # file name will be [Protocol]_[MarineGEO site code]_[data entry date in YYYY-MM-DD format]_[sheet]
          drop_upload(paste0(filenames$protocol[i], "_",
                             filenames$site[i], "_",
                             filenames$data_entry_date[i], "_", 
                             sheet, ".csv"),
                      path = paste0("Data/test_curated_directory/",
                                    project, "/",
                                    filenames$year[i], "/",
                                    filenames$site[i], "/",
                                    filenames$protocol[i], "/",
                                    sheet))
        }
      }
    }
    setwd(original_wd)
  }
  
## Helper functions ##########

# Reads in metadata from each file and generates standard filename
updateFileNames <- function(){

  # For each file uploaded: 
  for(i in 1:nrow(input$fileExcel)){
    # Extract the original file name to be saved in the submission log 
    filenames$original[i] <- input$fileExcel$name[i]
    
    # Import data
    # Format metadata to be more machine-readable
    # Turn to wide form, remove first row
    protocol_metadata <- read_xlsx(input$fileExcel$datapath[i], 
                                   sheet = "protocol_metadata", 
                                   col_names = c("category", "response"), skip=1) %>%
      spread(category, response) %>%
      mutate(data_entry_date = as.Date(as.numeric(data_entry_date), origin = "1899-12-30"))
    
    # Read in sample metadata to get site code
    sample_metadata <- read_xlsx(input$fileExcel$datapath[i], sheet = "sample_metadata")
    
    filenames$year[i] <- year(protocol_metadata$data_entry_date)
    filenames$protocol[i] <- protocol_metadata$protocol_name
    filenames$site[i] <- first(sample_metadata$site_code)
    filenames$data_entry_date <- protocol_metadata$data_entry_date

    # file name will be [Protocol]_[MarineGEO site code]_[data entry date in YYYY-MM-DD format]
    filenames$new[i] <- paste0(filenames$protocol[i], "_",
                               filenames$site[i], "_",
                               protocol_metadata$data_entry_date, ".xlsx")
    
    file.rename(input$fileExcel$datapath[i], filenames$new[i])
    
    }
}
  
# Called by the saveInitialData() function to acquire the submission log from DB and append new information to it  
generateSubmissionInfo <- function(){
  
  # Read in the submission log from Dropbox
  submission_log <- drop_read_csv("Data/submission_log.csv")

  # Collapse all variables to fit into a row in the submission log. 
  # They can be split by ; later 
  protocols <- paste(unique(filenames$protocol), collapse = "; ")
  original_filenames <- paste(filenames$original, collapse = "; ")  
  standardized_filenames <- paste(filenames$new, collapse="; ")
  emails <- tolower(input$email)
  
  # check which projects the emails provided with the submission are affiliated with
  project <- unique(filter(roster, email %in% emails)$project_affiliation)
  
  # If they're not affiliated, the curated submission will be sent to the unaffiliated submissions directory
  if(length(project)==0){
    project_affiliation$vector <- "unaffiliated_submissions"
  } else {
    project_affiliation$vector <- project
  }
  
  # Crate a new dataframe based on the number of emails provided 
  df <- setNames(data.frame(submission_time(), input$email, protocols, standardized_filenames, original_filenames, paste(project_affiliation$vector, collapse="; ")), 
                 c("submission_time", "email", "protocols", "standardized_filenames", "original_filenames", "project"))

  # Append the new data and send back to the dropbox upload function 
  rbind(submission_log, df)
}

# Test each file extension and ensure it's an xslx file
checkFileExtensions <- function(){

  if(any(!grepl("xlsx", input$fileExcel$name))){
    return(FALSE)
  } else return(TRUE)

}

testQA <- function(){
  
  # Create an empty list to hold all uploaded data
  # It will become a list of lists, where each first order list is a sheet in a protocol
  # And each second order list represents a protocol
  submission_data$all_data <-  vector("list", length(filenames$new))
  names(submission_data$all_data) <- filenames$protocol
  
  # Loop through each uploaded protocol
  for(i in 1:length(filenames$new)){
    current_protocol <- filenames$protocol[i]
    
    # Get names of sheets for given protocol
    protocol_sheets <- protocol_structure %>%
      filter(protocol == current_protocol) %$% # Note use of %$% rather than %>%, allows you to use $ in unique and get results as a vector
      unique(.$sheet)
    
    # Create an empty list, each object will be a sheet for the protocol
    protocol_df <- vector("list", length(protocol_sheets))
    names(protocol_df) <- protocol_sheets
    
    # Read in each sheet for the protocol, assign to respective list object 
    for(sheet_name in protocol_sheets) {
      protocol_df[[sheet_name]] <- read_excel(filenames$new[1], sheet = sheet_name)
    }
    
    ## TEST numeric type 
    # Get vector of numeric type columns in the given protocol
    numeric_columns <- protocol_structure %>%
      filter(protocol == current_protocol) %>%
      filter(type == "numeric") %$%
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
          
          QA_results$df[nrow(QA_results$df) + 1,] <- c("Test numeric variables", filenames$original[i], current_protocol, sheet_name, "Passed")
        },
        
        warning = function(w){
          QA_results$df[nrow(QA_results$df) + 1,] <<- c("Test numeric variables", filenames$original[i], current_protocol, sheet_name, unlist(w[1]))
        })
      }
    }
      
    # Add the protocol to the overall submission data list 
    submission_data$all_data[[current_protocol]] <- protocol_df
  
  }
  print(names(submission_data$all_data))
}

# Generate the QA report in markdown
renderReport <- function(){
  
  setwd(original_wd)
  
  # Check if the submission failed any QA tests
  if(any(QA_results$df$status != "Passed")){
    report_status(FALSE)
  } else report_status(TRUE)

  ## Write RMarkdown report #########

  rmarkdown::render(input = "./marinegeo_submission_report.Rmd",
                    output_format = "html_document",
                    output_file = paste0("submission_report_", submission_time(), ".html"),
                    output_dir = "./www/")
  
  
  report_path(paste0("submission_report_", submission_time(), ".html"))
  
  # Send the report to the dropbox
  drop_upload(paste0("./www/", report_path()),
              path = paste0("Data/submission_reports"))
  
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