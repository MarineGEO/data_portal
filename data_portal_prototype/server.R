# MarineGEO data submission app 
# This is the server script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu

function(input, output, session) {
  # Functions for extracting submission metadata
  source("extractSubmissionMetadata.R", local=TRUE)
  # QA functions
  source("./qaqc_scripts/id_relationship_test.R", local=TRUE)
  source("./qaqc_scripts/numeric_type_test.R", local=TRUE)
  source("./qaqc_scripts/date_format_test.R", local=TRUE)
  source("./qaqc_scripts/schema_evaluation.R", local=TRUE)

  # Submission time will store the time a user initially submits data using the humanTime function
  submission_time <- reactiveVal(0)
  # Holds current ID to be appended to QA 
  current_submission_id <- reactiveVal(NA)
  
  # Excel sheets with sensitive data must be tracked and stored separately from other files 
  # When users select the sensitive checkbox, this reactive value will change to TRUE
  sensitive <- reactiveVal()
  
  # Create empty list to hold original and standardized filenames of uploaded protocols + related metadata
  filenames <- reactiveValues()
  submission_metadata <- reactiveValues()
  # Dataframe to hold protocol metadata
  protocol_metadata <- reactiveValues(df = data.frame())
  
  # Create empty list to hold protocol data for QA testing and curation
  submission_data <- reactiveValues(all_data = list())
  
  # Dataframe holds all output from QAQC tests
  QA_results <- reactiveValues(df = tibble(test = NA_character_,
                                           filename = NA_character_,
                                           protocol = NA_character_,
                                           sheet = NA_character_,
                                           column = NA_character_,
                                           rows = NA_character_,
                                           values = NA_character_,
                                           submission_id = NA_character_,
                                           .rows=0))
  
  # Dataframes holds all metadata required to create output filenames and save data
  output_metadata <- reactiveValues(protocol = tibble(protocol = NA_character_,
                                                      filename = NA_character_,
                                                      site = NA_character_,
                                                      submission_id = NA_character_,
                                                      .rows = 0),
                                    table = tibble(protocol = NA_character_,
                                                   table = NA_character_,
                                                   filename = NA_character_,
                                                   submission_id = NA_character_,
                                                   valid_destination = NA,
                                                   .rows = 0))
                                    
  # Objects hold each protocol and related metadata as they undergo QAQC in called functions
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
    if(!no_db_testing){
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
  # Table on UI which reveals names of uploaded files. 
  output$uploaded <- renderTable({
    if(!is.null(input$fileExcel)){
      input$fileExcel %>%
        select(name) %>%
        rename(`Uploaded Files` = name)
    } else {
      tibble(`Uploaded Files` = "No files uploaded")
    }
  })
  
  # Return to data policy from submission page
  observeEvent(input$return_to_data_policy, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Policy")
  })
  
  ## ... submit data button - file save, QA, report display functions ##############
  # As long as the user has also provided an excel file and an email address, the initial file will be uploaded to a dropbox directory
  # QA/QC tests will be conducted on the file and a RMD report generated 
  # The user will be moved to a page with the report and whether their submission was successful
  observeEvent(input$submit, {
    
    # Put entire submission process inside a try catch 
    # This should prevent any unexpected errors from crashing the submission portal
    # tryCatch({
    # Get the current time
    submission_time(humanTime())
    
    # Create inital submission receipt (save email and submission time)
    if(!no_db_testing) initialReceipt() 
    
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
      
      # Extract essential metadata needed to classify that submission represents a MarineGEO protocol
      extractProtocolMetadata()

      # Upload initial files to dropbox and run QA checks
      saveInitialData()

      # Run beta QA process 
      QAQC()
      
      if(!no_db_testing) {
        saveCuratedData()
        saveSubmissionMetadata()
      }

      # Render the report and save to MarineGEO dropbox
      renderReport()
      
      # Move user to the data report page
      updateTabsetPanel(session, inputId = "nav", selected = "Data Report")
      
    } else {
      # This resets the file input but doesn't erase the file path, thus triggering errors even if the user then uploads the correct file type
      shinyjs::reset("fileExcel")
      
      showModal(modalDialog(
        title = "Failed Submission", 
        div(protocol_error_message()),
        
        easyClose = TRUE
      ))
    }
    # 
    # },
    # error = function(e){
    #   showModal(modalDialog(
    #     title = "An unexpected error occurred", 
    #     div("An unexpected error occurred.", 
    #         "Please email MarineGEO (marinegeo@si.edu) your Excel spreadsheets, and we will respond promptly.",
    #         "We apologize for this inconvenience."),
    #     
    #     easyClose = TRUE
    #   ))
    # })
  })
  
  ## ... data submission button lock conditions ###########
  # Prevent the "submit" button on the data submission page to be pressed if a user does not 
  # A. provide an email address. 
  # B. select if their data does or does not contain sensitive information 
  # C. upload an excel document 
  observe({
    if(!no_db_testing){
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
  # Render RMarkdown generated report
    
  output$html_report <- renderUI({
    if(file.exists(paste0("./www/", report_path()))){
      
      div(        
        includeHTML(paste0("./www/", report_path()))
      )
    }
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
    setwd(tempdir())
    #date <- as.character(humanTime())

    # If it doesn't, create 
    if(!no_db_testing){
      drop_create(path = paste0(destination, 
                                "initial_directory/", submission_time()))
    }
    
    # For each file uploaded:
    for(i in 1:nrow(input$fileExcel)){
      
      if(!(submission_metadata$original_filename[i] %in% protocol_metadata_error$df$filename)){
        attribute <- submission_metadata$new_filename[i]
      } else {
        attribute <- input$fileExcel$name[i]
        
        QA_results$df <- QA_results$df %>%
          add_row(test = "Upload failed initial processing",
                  filename = input$fileExcel$name[i])
      }
      
      if(!no_db_testing){
        # upload the initial data submission to dropbox
        drop_upload(attribute,
                    path = paste0(destination,
                                  "initial_directory/", submission_time()), mode = "add")
      }
    }
  }
  
  saveCuratedData <- function(){
    setwd(tempdir())
    
    tryCatch({
      
      # If this object is length 0, then no valid data has been uploaded
      if(length(submission_data$all_data) != 0){
        
        for(row in 1:nrow(output_metadata$protocol)){
          target_protocol <- pull(output_metadata$protocol[row,], protocol)
          target_site <- pull(output_metadata$protocol[row,], site)
          target_submission_id <- pull(output_metadata$protocol[row,], submission_id)
            
          for(target_table in names(submission_data$all_data[[row]])){
            target_filename <- paste(target_site,
                                     gsub("_", "-", target_protocol),
                                     gsub("_", "-", target_table),
                                     target_submission_id,
                                     sep="_")
            
            destination_check <- protocol_structure %>%
              select(protocol, table) %>%
              distinct() %>%
              filter(protocol == target_protocol,
                     table == target_table)
            
            # Write the curated set to the directory and send to Dropbox
            write_csv(submission_data$all_data[[row]][[target_table]],
                      paste0(target_filename, ".csv"))
            
            if(nrow(destination_check) == 1){
              drop_upload(paste0(target_filename, ".csv"),
                          path = paste0(destination,
                                        "L0_directory/",
                                        target_protocol, "/",
                                        target_table))
              
              output_metadata$table <- output_metadata$table %>%
                add_row(protocol = target_protocol,
                        table = target_table,
                        submission_id = target_submission_id,
                        filename = paste0(target_filename, ".csv"),
                        valid_destination = TRUE)
              
            } else {
              drop_upload(paste0(target_filename, ".csv"),
                          path = paste0(destination,
                                        "L0_directory/invalid_tables"))
              
              output_metadata$table <- output_metadata$table %>%
                add_row(protocol = target_protocol,
                        table = target_table,
                        submission_id = target_submission_id,
                        filename = paste0(target_filename, ".csv"),
                        valid_destination = FALSE)
            }
          }
        }
      } else {
        QA_results$df <- QA_results$df %>%
          add_row(test = "No valid data uploaded")
        
      }
    },
    
    error = function(e){
      
      print(e)
      
      # Create an error message in the QA result log
      QA_results$df <- QA_results$df %>%
        add_row(test = "Error transmitting curated data")
    })
    
    setwd(original_wd)
  }
  

## Helper functions ##########
# Records any submission attempt
# Allows MarineGEO to know someone is trying to submit data in case they are getting errors that crash the application
initialReceipt <- function(){
  setwd(tempdir())
  
  writeLines(input$email, paste0(submission_time(), ".txt"))
  drop_upload(paste0(submission_time(), ".txt"), 
                     path = paste0(destination,
                                   "submission_reports/initial_submission_receipts"), mode = "overwrite")
  
  setwd(original_wd)
}  
  
# Test each file extension and ensure it's an xslx file
checkFileExtensions <- function(){
  
  if(any(!grepl("xlsx", input$fileExcel$name))){
    # Update protocol error message with proper warning
    protocol_error_message(filter(warnings, title == "no_excel_files")$message)
    return(FALSE)
    
  } else return(TRUE)
}

QAQC <- function(){
  # Data will be read in, tested, and stored in submission_data$all_data
  # Submission_data$all_data is a list of lists, where each first order list is a sheet in a protocol
  # And each second order list represents a protocol
  
  # Loop through each uploaded protocol
  for(i in 1:length(submission_metadata$original_filename)){
    
    tryCatch({

      original_filename_qa(submission_metadata$original_filename[i])
      current_protocol(submission_metadata$protocol[i])
      
      # If protocol name is out of date, update it. 
      current_protocol(
        case_when(
          current_protocol() == "fish_seines" ~ "beach_seines",
          current_protocol() == "water_quality" ~ "environmental_monitoring",
          current_protocol() == "seagrass_organic_matter" ~ "sediment_organic_matter",
          TRUE ~ current_protocol()
        )
      )
      
      # Only submissions with a valid protocol name will go through the QAQC process and be saved as CSVs
      # This includes deprecated protocol names (water quality, fish seines, seagrass_organic_matter, etc.)
      if(current_protocol() %in% protocol_structure$protocol){
        
        # Get names of sheets present in the file
        available_sheets <- excel_sheets(submission_metadata$new_filename[i])
        
        # Remove unnecessary sheets (protocol_metadata and glossary)
        protocol_sheets(available_sheets[!(available_sheets %in% c("protocol_metadata", "glossary"))])
        
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
          if(nrow(df) > 0){
            protocol_df[[sheet_name]] <- df %>%
              mutate(submission_id = paste(submission_time(), i, sep = "_"),
                     protocol_id = current_protocol(),
                     table_id = sheet_name)
            
            current_submission_id(paste(submission_time(), i, sep = "_"))
          
            } else{
              # Remove the 0 row table from the protocol sheets object
            protocol_sheets_non_reactive <- protocol_sheets()
            protocol_sheets(protocol_sheets_non_reactive[protocol_sheets_non_reactive != sheet_name])
          }
        }
        
        # Remove any NULL items from the list that are the result of an empty data frame
        protocol_df <- compact(protocol_df)
        
        # If an empty dataframe has been uploaded, skip to the next submitted object 
        if(length(protocol_df) == 0){next}
        
        # Save protocol data to a reactive list object so QA tests can access within environment
        stored_protocol$df <- protocol_df
        
        ## ... Call quality control tests ####
        # Schema QC tests
        schemaTableNames()
        schemaColumnNames()        
        
        # Data type tests
        evaluateDates()
        testNumericType()
        # numericMinMaxTest()
        
        # Check relationships between tables
        checkIDRelationships()

        # Add the protocol to the overall submission data list 
        submission_data$all_data[[paste(current_protocol(), 
                                        submission_metadata$original_filename[i], 
                                        submission_metadata$site[i], sep="_")]] <- protocol_df
        
        # Save required metadata to create output and filename
        output_metadata$protocol <- output_metadata$protocol %>%
          add_row(protocol = current_protocol(),
                  filename = submission_metadata$original_filename[i],
                  site = submission_metadata$site[i],
                  submission_id = paste(submission_time(), i, sep = "_"))
        
      # Create a warning if an invalid or missing protocol name was provided
      } else {
        QA_results$df <- QA_results$df %>%
          add_row(test = "Invalid or missing protocol name",
                  protocol = current_protocol(),
                  filename = original_filename_qa()) 
      }
    },

    error = function(e){
      
      print(e)

      # Create an error message in the QA result log
      QA_results$df <- QA_results$df %>%
        add_row(test = "Error during QAQC tests",
               protocol = submission_metadata$protocol[i],
               filename = submission_metadata$original_filename[i])
    })
  }
}

# Called by the saveInitialData() function to acquire the submission log from DB and append new information to it  
saveSubmissionMetadata <- function(){
  setwd(tempdir())
  
  tryCatch({
    
  # Access the submission log from dropbox and append current emails/time/datafile name
  submission_log <- drop_read_csv(paste0(destination, "submission_log.csv"))
  
  submission_log <- submission_log %>%
    mutate_all(as.character) %>%
    add_row(  
      original_filenames = paste(submission_metadata$original_filename, collapse = "; "),  
      protocols = paste(unique(output_metadata$protocol$protocol), collapse = ";"),
      standardized_filenames = paste(output_metadata$table$filename, collapse="; "),
      email = tolower(trimws(unlist(strsplit(input$email, ";")))),
      submission_time = submission_time(),
      valid_destination = as.character(all(output_metadata$table$valid_destination)),
      qaqc_errors = as.character(nrow(QA_results$df) > 0)
    )

  submission_log_path <- file.path("submission_log.csv")
  write.csv(submission_log, submission_log_path, row.names = FALSE, quote = TRUE)
  drop_upload(submission_log_path, path = destination, mode = "overwrite")
  
  # Deposit QA results and protocol metadata tables
  QA_results$df <- mutate(QA_results$df, submission_time = submission_time())
  output_metadata$table <- mutate(output_metadata$table, submission_time = submission_time())
  
  write_csv(QA_results$df, paste0("qc_", submission_time(), ".csv"))
  drop_upload(paste0("qc_", submission_time(), ".csv"),
              path = paste0(destination,
                            "resources/quality_control_results"))
  
  if(nrow(output_metadata$table) > 0){
    write_csv(output_metadata$table, paste0("table_metadata_", submission_time(), ".csv"))
    drop_upload(paste0("table_metadata_", submission_time(), ".csv"),
                path = paste0(destination,
                              "resources/output_metadata"))
  }
  
  if(nrow(protocol_metadata$df) > 0){
    write_csv(protocol_metadata$df, paste0("protocol_metadata", "_", submission_time(), ".csv"))
    drop_upload(paste0("protocol_metadata", "_", submission_time(), ".csv"),
                path = paste0(destination,
                              "resources/protocol_metadata"))
  }   
  
  },
  
  error = function(e){
    
    print(e)
    
    # Create an error message in the QA result log
    QA_results$df <- QA_results$df %>%
      add_row(test = "Error transmitting submission metadata")
  })
  
  setwd(original_wd)
}

# Generate the QA report in markdown
renderReport <- function(){
  
  setwd(original_wd)
  
  tryCatch({
    
    # Check if the submission failed any QA tests
    if(nrow(QA_results$df) == 0){
      report_status("Submission successful")
    } else{
      report_status("Some files did not pass all tests")
    }
    
    ## Write RMarkdown report #########
    rmarkdown::render(input = "./marinegeo_submission_report.Rmd",
                      output_format = "html_document",
                      output_file = paste0("submission_report_", submission_time(), ".html"),
                      output_dir = "./www/")
    
    report_path(paste0("submission_report_", submission_time(), ".html"))
    
    if(!no_db_testing){
      # Send the report to the dropbox
      drop_upload(paste0("./www/", report_path()),
                  path = paste0(destination, "submission_reports"))
    }
    
  },
  
  error = function(e){
    
    print(e)
    
    # Create an error message in the QA result log
    QA_results$df <- QA_results$df %>%
      add_row(test = "Error creating RMarkdown report")
  })
  
  
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