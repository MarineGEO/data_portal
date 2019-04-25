# Coastal Carbon Research Coordination Network
# This is the server script for three-file version of the CCRCN data submission application
# Contact: Michael Lonneman, lonnemanM@si.edu
#          Dave Klinges, klingesD@si.edu


function(input, output, session) {
  
  ## 1. Initialize data tables ###############
  # Each of these reactive data objects will track user inputs
  # and influence what options will appear throughout the submission process
  # By default, they are blank. 
  # If a user selects to continue a past data submission entry or to view/edit
  # the metadata of a study that is already in the Library, observeEvent calls 
  # will populate these tables with the relevant data. 
  study_information <- reactiveValues(df = study_information)
  authors <- reactiveValues(df = authors)
  keywords <- reactiveValues(df = data.frame())
  associated_publications <- reactiveValues(df = associated_publications)
  methods_metadata <- reactiveValues(df = data.frame())
  
  # The datatype tracker monitors which datatypes the user has data for in order to populate the templates
  datatype_tracker <- reactiveValues(df = datatype_tracker)
  
  # A blank dataframe that is used to fill out UI outputs when editing previously entered authors 
  edited_author <- reactiveValues(df = initial_edited_author)
  # tracks whether the user currently editing an author
  edit_author_mode <- reactiveVal(FALSE)
  # authorID assigns a unique identifier to each unique author in order to track edits
  # Once a user completes editing an author, that row will be updated with new information
  # This is to avoid duplicate author rows being added to the author metadata table
  authorID <- reactiveVal(1)
  
  # Blank templates 
  depthseries_template <- reactiveValues(df = data.frame())
  site_template <- reactiveValues(df = data.frame())
  
  # Objective tracks the use case of the user: new data, revise existing metadata, continue data submission
  objective <- reactiveVal("new_submission")
  
  # If the user wants to submit new data, all UI prompts are empty or listed as "not specified"
  observeEvent( input$new_submission, {
    objective("new_submission")
    updateTabsetPanel(session, inputId = "nav", selected = "Study Information")
  })
  
  # Currently these are not functional
  # Edit existing metadata
  observeEvent(input$edit_metadata, {
    objective("edit_metadata")
  })
  
  output$edit_study <- renderUI({
    if(objective() == "edit_metadata"){
      selectInput("edit_study", "Select a study", choices = unique(metadata$study_id))
    }
  })
  
  # View data policy
  observeEvent(input$data_policy_intro, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Policy")
  })
  
  observeEvent(input$data_policy, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Policy")
  })
  
  ## 2. Render UI elements ################
  # All of the input box contents must be handled by the server since users 
  # can either submit new data or revise past submissions. 
  
  # All render UI boxes check whether it is a new submission or revision of existing metadata. 
  # The objective() updates based on user selections on the "welcome" page. 
  
  ## ... 2a Render Study Information ######
  # Title, one-liner, absract, start and end date
  output$title <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("title", mandatoryLabel("Title of dataset"), width="80%")

    # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
    # Continue new data submission:
    } else {
      
    }
  })
  
  output$one_liner <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("one_liner", "Provide a one-line description of your data", width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$abstract <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textAreaInput("abstract", "Provide the abstract for your data", width="145%",
                    rows=6)
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$start_date <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      dateInput("start_date", "Start date of study", format = "yyyy-mm-dd")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$end_date <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      dateInput("end_date", "End date of study", format = "yyyy-mm-dd")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... 2b Authors & Contact Information #############
  output$last_name <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("last_name", "Last name of author", value = getAuthorValue(edited_author$df$last_name), width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$given_name <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("given_name", "Given name of author", value = getAuthorValue(edited_author$df$given_name), width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$institution <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("institution", "Institutional affiliation", value = getAuthorValue(edited_author$df$institution), width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$email <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("email", "Email address", value = getAuthorValue(edited_author$df$email), width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$address <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("address", "Mailing address", value = getAuthorValue(edited_author$df$address), width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$phone <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("phone", "Phone number", value = getAuthorValue(edited_author$df$phone), width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$corresponding_author <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      # getAuthorValue can't be used here because value requires T or F
      checkboxInput("corresponding_author", "Is this the corresponding author for your data release?", 
                    value = 
                      if(nrow(edited_author$df)==0){ 
                        FALSE
                      } else { 
                        as.logical(edited_author$df$corresponding_author) 
                        }
                    )
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$author_list <- renderUI({
    # Show box as long as there is at least one author added to the author dataframe: 
    if(length(authors$df$last_name) > 0) {
      # Eventually paste together last name, given name for the choices
      selectInput("author_list", "Select an author to edit", choices = unique(authors$df$last_name))
    }
  })
  
  output$edit_author <- renderUI({
    if(length(authors$df$last_name) > 0) {
      actionButton("edit_author", "Edit selected author")
    }
  })
  
  output$add_author <- renderUI({
    if(edit_author_mode() == FALSE){
      actionButton("add_author", "Confirm and add additional author", 
                  class = "btn-primary")
    } 
  })
  
  output$confirm_author_edits <- renderUI({
    if(edit_author_mode() == TRUE){
          actionButton("confirm_author_edits", "Confirm edits to author", 
                   class = "btn-primary")
    }
  })
  
  output$add_last_author <- renderUI({
    if(edit_author_mode() == FALSE){
      actionButton("add_last_author", "Confirm and close author's table",
                  class = "btn-primary")
    }
  })
  
  ## ... 2c Keywords ###########
  output$keywords <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("keywords", "Enter keywords associated with your data. 
                Separate multiple keywords with semicolons",
                width="80%")      
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... 2d Associated Publications ###########
  
  output$title_pubs <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("title_pubs", "Title", width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$doi_pubs <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("doi_pubs", "DOI", width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$bibtex_pubs <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("bibtex_pubs", "BibTeX citation", width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... 2e Render Methods Metadata ############
  
  ## ... ... Universal variables ######
  output$coring_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("coring_method", choices = coring_methods_var, 
                  label=NULL, selected = NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      selectInput("coring_method", choices = coring_methods_var, 
                  label=NULL, width = "80%",
                  selected = filter(metadata, study_id == input$edit_study)$coring_method)
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$roots_flag <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("roots_flag", choices = c("not specified", "roots and rhizomes included", "roots and rhizomes separated"), 
                  label=NULL, selected = NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$sediment_sieve_size <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("sediment_sieve_size", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$compaction_flag <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("compaction_flag", choices = compaction_flag_var,
                  label=NULL, selected = NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$carbon_profile_notes <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("carbon_profile_notes", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... ... LOI #####################
  output$loi_temp <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("loi_temp", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$loi_time <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("loi_time", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$loi_sample_volume <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("loi_sample_volume", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$loi_sample_mass <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("loi_sample_mass", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$loi_approx <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      checkboxInput("loi_approx", "LOI time recorded herein is an approximate estimation")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... ... bulk density #####################
  output$dbd_temp <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("dbd_temp", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$dbd_time <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("dbd_time", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$dbd_sample_volume <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("dbd_sample_volume", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$dbd_sample_mass <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("dbd_sample_mass", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$dbd_density <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("dbd_density", label=NULL, choices = dbd_density_var, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... ... Fraction organic matter #############
  
  output$carbonates_removed <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & is.null(input$carbon_measured_or_modeled) == FALSE){
      if(input$carbon_measured_or_modeled == "measured"){
        div(id = "carbonates_removed",
            checkboxInput("carbonates_removed", 
                    "Were carbonates removed prior to calculating fraction organic carbon?",
                    width = "80%")
        )
      }
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
    
  })
  
  output$carbonate_removal_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & is.null(input$carbon_measured_or_modeled) == FALSE){
      if(input$carbon_measured_or_modeled == "measured"){
        div(id = "carbonates_removed",
            "Specify the method used to remove carbonates:",
            selectInput("carbonate_removal_method", label=NULL, 
                        choices = c("not specified", "direct acid treatment", "acid fumigation", "low carbonate soil", 
                                    "carbonates not removed"), width = "80%")
        )
      }
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... ... Fraction Carbon ############
  output$carbon_measured_or_modeled <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("carbon_measured_or_modeled", label=NULL, choices = c("not specified", "measured", "modeled"), width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$fraction_carbon_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("fraction_carbon_method", label=NULL, choices = fraction_carbon_method_var, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$fraction_carbon_type <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("fraction_carbon_type", label=NULL, choices = c("not specified", "organic carbon", "total carbon"), width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... ... Age-Depth Models ############
  output$cs137_counting_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("cs137_counting_method", label=NULL, choices = c("not specified", "alpha", "gamma"), width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$pb210_counting_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("pb210_counting_method", label=NULL, choices = c("not specified", "alpha", "gamma"), width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$excess_pb210_exists <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      checkboxInput("excess_pb210_exists", label="Does the study have an excess lead 210 model?")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
    
  })
  
  output$excess_pb210_rate <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & is.null(input$excess_pb210_exists) == FALSE){
      if(input$excess_pb210_exists == TRUE){
        div(id = "pb210_rate",
            "Specify the mass or accretion rate used in the excess lead 210 model",
            selectInput("pb210_counting_method", label=NULL, choices = c("not specified", "mass accumulation", "accretion"), 
                  width = "80%")
        )
      }
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$excess_pb210_model <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & is.null(input$excess_pb210_exists) == FALSE){
      if(input$excess_pb210_exists == TRUE){
        div(id = "pb210_rate",
            "Specify the model used to estimate excess lead 210",
            selectInput("pb210_counting_method", label=NULL, choices = c("not specified", "CRS", "CIC", "CFCS")) 
        )
      }
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$ra226_assumption <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("ra226_assumption", label=NULL, choices = c("not specified", "each sample", 
                                                                   "total core", "at asymptote")) 
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$c14_counting_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("c14_counting_method", label=NULL, choices = c("not specified", "AMS", "beta")) 
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
    
  })
  
  output$other_marker <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      checkboxInput("other_marker", label="Did you date any other depth horizons, such as an artificial marker, pollen horizon, 
                    or pollution horizon?") 
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
    
  })
  
  output$age_depth_exists <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      checkboxInput("age_depth_exists", label="Did the study build full age depth models?", width = "80%") 
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
    
  })
  
  output$dating_notes <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & is.null(input$age_depth_exists) == FALSE){
      if(input$age_depth_exists == TRUE){
        div(id = "dating_notes",
            "Provide any additional notes on the process of dating the core",
            textInput("dating_notes", label=NULL) 
        )
      }
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
    
  })
  
  output$age_depth_model_reference <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & is.null(input$age_depth_exists) == FALSE){
      if(input$age_depth_exists == TRUE){
        div(id = "dating_notes",
            "Indicate the reference or 0 year of the age depth model", 
            selectInput("age_depth_model_reference", label=NULL, choices = c("YBP", "CE", "core collection date")) 
        )
      }
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
    
  })
  
  output$age_depth_model_notes <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("age_depth_model_notes", label=NULL) 
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
    
  })
  
  ## ... 2f Site-level metadata ##################
  output$site_ids <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("site_ids", label="", width="80%")      
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$site_salinity_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & input$site_salinity_check == "Yes"){
      
      div(id = "salinity", 
          "Was salinity determined using field observations or a measurement?",
          selectInput("site_salinity_method", label = NULL, 
                      selected = "not specified", 
                      choices = c("not specified", "field observation", "measurement"), width="80%"))      
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$site_vegetation_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & input$site_vegetation_check == "Yes"){
      
      div(id = "vegetation", 
          "Was vegetation determined using field observations or a measurement?",
          selectInput("site_vegetation_method", label = NULL, 
                      selected = "not specified", 
                      choices = c("not specified", "field observation", "measurement"), width="80%"))      
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$site_inundation_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & input$site_inundation_check == "Yes"){
      
      div(id = "inundation", 
          "Was inundation determined using field observations or a measurement?",
          selectInput("site_inundation_method", label = NULL, 
                      selected = "not specified", 
                      choices = c("not specified", "field observation", "measurement"), width="80%"))      
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... 2g Core-level metadata ##################
  output$core_ids <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("core_ids", label="", width="80%")      
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$core_impact_class <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      checkboxInput("core_impact_class", label="Did you document any major anthropogenic impacts historically 
                    and currently affecting the coring location?",
                    value = FALSE, width="80%")      
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... ... core position UI #############
  output$core_position_varies <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      checkboxInput("core_position_varies", label="Each core position was measured using the same method", 
                    value = TRUE, width="80%")      
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$core_position <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & is.null(input$core_position_varies) == FALSE){
      if(input$core_position_varies == TRUE){
        div(id = "core_position_method",
            "How did you measure core position?",
            selectInput("core_position", label = NULL, choices = core_position_var,
                        selected = "Not specified", width="80%"))
      }
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$core_position_accuracy <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & is.null(input$core_position_varies) == FALSE){
      if(input$core_position_varies == TRUE){
        div(id="core_accuracy", 
          "Accuracy of latitude and longitude measurement, if determined", tags$br(), 
          textInput("core_position_accuracy", label = NULL, placeholder = "meters")      
        )
      }
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... ... core elevation UI ###############
  
  output$core_elevation_varies <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      checkboxInput("core_elevation_varies", label="Elevation was determined using the same method for each core",
                    value = TRUE, width="80%")      
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$core_elevation_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & is.null(input$core_elevation_varies) == FALSE){
      if(input$core_elevation_varies == TRUE){
        div(id = "core_elevation_table",
            "How did you measure core elevation?", 
            selectInput("core_elevation_method", label = NULL, 
                       selected = "not specified", choices = core_elevation_var, width="80%")
          )      
      }
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$core_elevation_accuracy <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & is.null(input$core_elevation_varies) == FALSE){
      if(input$core_elevation_varies == TRUE){
        div(id = "core_elevation_table",
            "Accuracy of core elevation measurement, if determined", tags$br(),
            textInput("core_elevation_accuracy", label = NULL, placeholder = "meters")      
        )
      }
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$core_elevation_datum <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & is.null(input$core_elevation_varies) == FALSE){
      if(input$core_elevation_varies == TRUE){
        div(id = "core_elevation_table",
            "What datum did you measure core elevation against?", 
            selectInput("core_elevation_datum", label = NULL, 
                        selected = "Not specified", choices = c("Not specified", "NAVD88", "MSL", "MTL", "MHW", "MHHW",
                                                          "MHHWS", "MLW", "MLLW"), width="80%")
        )
      }
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... ... core salinity, vegetation, and inundation ########
  output$core_salinity_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & input$core_salinity_check == "Yes"){
      
      div(id = "salinity", 
          "Was salinity determined using field observations or a measurement?",
          selectInput("core_salinity_method", label = NULL, 
                  selected = "not specified", 
                  choices = c("not specified", "field observation", "measurement"), width="80%"))      
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$core_vegetation_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & input$core_vegetation_check == "Yes"){
      
      div(id = "vegetation", 
          "Was vegetation determined using field observations or a measurement?",
          selectInput("core_vegetation_method", label = NULL, 
                      selected = "not specified", 
                      choices = c("not specified", "field observation", "measurement"), width="80%"))      
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$core_inundation_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission" & input$core_inundation_check == "Yes"){
      
      div(id = "inundation", 
          "Was inundation determined using field observations or a measurement?",
          selectInput("core_inundation_method", label = NULL, 
                      selected = "not specified", 
                      choices = c("not specified", "field observation", "measurement"), width="80%"))      
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## 3. Action logic and saving data to tables ################
  # logic executed when action buttons/checkboxes/collapse panel headers are used
  
  ## ... 3a Confirm Study Information ########
  observeEvent(input$add_study_information, {
    # Each time submit is clicked, re-create the study information table 
    # This will allow users to update one or more variables after initial submission of the table
    study_information$df <- as.data.frame(t(sapply(study_information_var, function(x) input[[x]])))
    
    # Open the authors panel and close the study information panel
    updateCollapse(session, id = "study_information_bspanel", close = "Study Information")
    updateCollapse(session, id = "authors_bspanel", open = "Authors")
  })
  
  ## ... 3b Confirm author and add additional authors #######
  # When an author is added, add information to new row and clear fields to add another author
  observeEvent(input$add_author, {
    # gather up author information and organize in a table
    author <- as.data.frame(t(sapply(authors_var, function(x) input[[x]])))
    
    # assign and update author ID variable
    author <- mutate(author, id = authorID())
    authorID(authorID() + 1)
    
    # add new row to authors table
    authors$df <- bind_rows(authors$df, author)
    
    # reset form info
    shinyjs::reset("authors_table")
  })
  
  ## ... 3b ... Confirm author and move on to keywords #######
  # When an author is added, add information to new row and clear fields to add another author
  observeEvent(input$add_last_author, {
    # gather up author information and organize in a table
    author <- as.data.frame(t(sapply(authors_var, function(x) input[[x]])))
    
    # if no name was provided no new author is added
    if(author$last_name != "") {
      # assign and update author ID variable
      author <- mutate(author, id = authorID())
      authorID(authorID() + 1)
      
      # add new row to authors table
      authors$df <- bind_rows(authors$df, author)
    }

    # reset form info
    shinyjs::reset("authors_table")
    updateCollapse(session, id = "authors_bspanel", close = "Authors")
    updateCollapse(session, id = "keywords_bspanel", open = "Keywords")
  })
  
  ## ... 3b ... Edit author ###########################
  # Isolate the author to be edited
  # The render UI boxes should automatically update once edited_author changes
  
  observeEvent(input$edit_author, {
    edited_author$df <- filter(authors$df, last_name == input$author_list)
    edit_author_mode(TRUE)  
  })
  
  observeEvent(input$confirm_author_edits, {
    edit_id <- edited_author$df$id
    author <- as.data.frame(t(sapply(authors_var, function(x) input[[x]])))
    author <- mutate(author, id = edit_id)
    # remove old author row and add revised details
    authors$df <- authors$df %>%
      filter(id != edit_id) %>%
      bind_rows(author)
    
    # turn edit mode off, clear author info from edit table 
    edit_author_mode(FALSE)
    edited_author$df <- initial_edited_author
    shinyjs::reset("authors_table")
    
  })

  ## ... 3c Confirm keywords #######
  observeEvent(input$add_keywords, {
    # gather up keywords and organize in a table
    keywords_vector <- strsplit(input$keywords, ";")
    keywords$df <- as.data.frame(keywords_vector, col.names = c("key_words"))
    
    updateCollapse(session, id = "keywords_bspanel", close = "Keywords")
    updateCollapse(session, id = "associated_pubs_bspanel", open = "Associated Publications")
  })
  
  ## ... 3d Confirm associated publications and add additional publications ########
  # When an associated publication is added, add information to new row and clear fields to add another associated publication
  observeEvent(input$add_pub, {
    # gather up publication information and organize in a table
    publication <- as.data.frame(t(sapply(associated_publications_var, function(x) input[[x]])))
    # add new row to publication table
    associated_publications$df <- bind_rows(associated_publications$df, publication)
    
    # reset form info
    shinyjs::reset("associated_publications_table")
    
    # updateCollapse(session, id = "associated_pubs_bspanel", close = "Associated Publications")
  })
  
  observeEvent(input$add_last_pub, {
    # gather up publication information and organize in a table
    publication <- as.data.frame(t(sapply(associated_publications_var, function(x) input[[x]])))
    # add new row to authors table
    associated_publications$df <- bind_rows(associated_publications$df, publication)
    
    # reset form info
    shinyjs::reset("associated_publications_table")
    updateCollapse(session, id = "associated_pubs_bspanel", close = "Associated Publications")

  })
  
  # Confirm all study information 
  # This button simply changes the tabpanel that's open
  observeEvent(input$confirm_study_tab, {
    updateTabsetPanel(session, inputId = "nav", selected = "Methods and Materials Metadata")
  })
  
  ## ... 3e Methods Metadata ##############
  
  ## ... ... Universal Metadata #########
  observeEvent(input$confirm_coring_methods, {
    updateCollapse(session, id="core_methods_bspanel", close = "Coring Methods")
  })
  
  ## ... ... Carbon Stock Metadata #####
  observeEvent(input$confirm_organic_carbon, {
    updateCollapse(session, id="carbon_measurements_bspanel", close = "Carbon Stocks")
  })
  
  observeEvent(input$cancel_organic_carbon, {
    # reset variables
    shinyjs::reset("loi")
    shinyjs::reset("dbd")
    shinyjs::reset("fc")
    shinyjs::reset("foc")
    
    # close the panel
    
    updateCollapse(session, id="carbon_measurements_bspanel", close = "Carbon Stocks")
  })
  
  # LOI variables: 
  observe({
    if(input$loi == TRUE){
      updateCollapse(session, id = "carbon_measurements_bspanel", open = "loi_var_bspanel")
    }  else {
      updateCollapse(session, id = "carbon_measurements_bspanel", close = "loi_var_bspanel")
    }
  })
  
  observeEvent(input$confirm_loi, {
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "loi_var_bspanel")
    datatype_tracker$df$loi <- TRUE
  })
  
  observeEvent(input$cancel_loi, {
    # reset LOI variables
    shinyjs::reset("loi_table")
    # unselect the LOI checkbox
    shinyjs::reset("loi")
    # close the panel
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "loi_var_bspanel")
  })
  
  # DBD variables: 
  observe({
    if(input$dbd == TRUE){
      updateCollapse(session, id = "carbon_measurements_bspanel", open = "dbd_bspanel")
    } else {
      updateCollapse(session, id = "carbon_measurements_bspanel", close = "dbd_bspanel")
    } 
  })
  
  observeEvent(input$confirm_dbd, {
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "dbd_bspanel")
    datatype_tracker$df$dbd <- TRUE
  })
  
  observeEvent(input$cancel_dbd, {
    # reset dbd variables
    shinyjs::reset("dbd_table")
    # unselect the dbd checkbox
    shinyjs::reset("dbd")
    # close the panel
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "dbd_bspanel")
  })
  
  # Fraction Carbon:
  observe({
    if(input$fraction_carbon == TRUE){
      updateCollapse(session, id = "carbon_measurements_bspanel", open = "fraction_carbon_bspanel")
    } else {
      updateCollapse(session, id = "carbon_measurements_bspanel", close = "fraction_carbon_bspanel")
    } 
  })
  
  observeEvent(input$confirm_fc, {
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "fraction_carbon_bspanel")
    datatype_tracker$df$fraction_carbon <- TRUE
  })
  
  observeEvent(input$cancel_fc, {
    # reset fc variables
    shinyjs::reset("fraction_carbon_table")
    # unselect the fc checkbox
    shinyjs::reset("fraction_carbon")
    # close the panel
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "fraction_carbon_bspanel")
  })
  
  ## ... ... Age-Depth Metadata #######
  # cs137
  
  observe({
    if(input$cs137 == TRUE){
      updateCollapse(session, id = "dating_methods_bspanel", open = "cs137_bspanel")
    } else {
      updateCollapse(session, id = "dating_methods_bspanel", close = "cs137_bspanel")
    } 
  })
  
  observeEvent(input$confirm_cs137, {
    updateCollapse(session, id = "dating_methods_bspanel", close = "cs137_bspanel")
    datatype_tracker$df$cs137 <- TRUE
  })
  
  observeEvent(input$cancel_cs137, {
    # reset cs137 variables
    shinyjs::reset("cs137_table")
    # unselect the cs137 checkbox
    shinyjs::reset("cs137")
    # close the panel
    updateCollapse(session, id = "dating_methods_bspanel", close = "cs137_bspanel")
  })
  
  # pb210
  observe({
    if(input$pb210 == TRUE){
      updateCollapse(session, id = "dating_methods_bspanel", open = "pb210_bspanel")
    } else {
      updateCollapse(session, id = "dating_methods_bspanel", close = "pb210_bspanel")
    } 
  })
  
  observeEvent(input$confirm_pb210, {
    updateCollapse(session, id = "dating_methods_bspanel", close = "pb210_bspanel")
    datatype_tracker$df$pb210 <- TRUE
  })
  
  observeEvent(input$cancel_pb210, {
    # reset pb210 variables
    shinyjs::reset("pb210_table")
    # unselect the pb210 checkbox
    shinyjs::reset("pb210")
    # close the panel
    updateCollapse(session, id = "dating_methods_bspanel", close = "pb210_bspanel")
  })
  
  # c14
  observe({
    if(input$c14 == TRUE){
      updateCollapse(session, id = "dating_methods_bspanel", open = "c14_bspanel")
    } else {
      updateCollapse(session, id = "dating_methods_bspanel", close = "c14_bspanel")
    } 
  })
  
  observeEvent(input$confirm_c14, {
    updateCollapse(session, id = "dating_methods_bspanel", close = "c14_bspanel")
    datatype_tracker$df$c14 <- TRUE
    
  })
  
  observeEvent(input$cancel_c14, {
    # reset dbd variables
    shinyjs::reset("c14_table")
    # unselect the dbd checkbox
    shinyjs::reset("c14")
    # close the panel
    updateCollapse(session, id = "dating_methods_bspanel", close = "c14_bspanel")
  })
  
  observeEvent(input$be7, {
    if(input$be7 == TRUE) {
      datatype_tracker$df$be7 <- TRUE
    } else {     
      datatype_tracker$df$be7 <- FALSE
    }
  })
  
  observeEvent(input$am241, {
    if(input$am241 == TRUE) {
      datatype_tracker$df$am241 <- TRUE
    } else {     
      datatype_tracker$df$am241 <- FALSE
    }
  })
  
  # cancel or confirm all age depth choices
  
  observeEvent(input$cancel_age_depth, {
    # reset LOI variables
    shinyjs::reset("age_depth_types")
    # unselect the LOI checkbox
    shinyjs::reset("age_depth")
    # close the panel
    updateCollapse(session, id = "dating_methods_bspanel", close = "ad_types_bspanel")
  })
  
  observeEvent(input$confirm_age_depth, {
    updateCollapse(session, id = "dating_methods_bspanel", close = "ad_types_bspanel")
  })
  
  # confirm all material and method metadata selections
  observeEvent(input$submit_methods_metadata, {

    updateTabsetPanel(session, inputId = "nav", selected = "Site and Core Metadata")
    
    methods_metadata$df <- initial_methods_metadata %>%
      mutate(coring_method = input$coring_method, roots_flag = input$roots_flag, sediment_sieved_flag = input$sediment_sieved_flag, compaction_flag = input$compaction_flag,
             dry_bulk_density_temperature = input$dbd_temp, dry_bulk_density_time = input$dbd_time, 
             dry_bulk_density_sample_volume = input$dbd_sample_volume, dry_bulk_density_sample_mass = input$dbd_sample_mass, dry_bulk_density_flag = input$dbd_density,
             loss_on_ignition_temperature = input$loi_temp, loss_on_ignition_time = input$loi_time, 
             loss_on_ignition_flag = input$loi_approx, loss_on_ignition_sample_volume = input$loi_sample_volume, loss_on_ignition_sample_mass = input$loi_sample_mass,
             carbonates_removed = input$carbonates_removed, carbonate_removal_method = input$carbonate_removal_method,
             carbon_measured_or_modeled = input$carbon_measured_or_modeled, fraction_carbon_method = input$fraction_carbon_method, 
             fraction_carbon_type = input$fraction_carbon_type, carbon_profile_notes = input$carbon_profile_notes,
             cs137_counting_method = input$cs137_counting_method,              
             pb210_counting_method = input$pb210_counting_method, excess_pb210_rate = input$excess_pb210_rate, excess_pb210_model = input$excess_pb210_model, 
             ra226_assumption = input$ra226_assumption, 
             cs14_counting_method = input$c14_counting_method, 
             dating_notes = input$dating_notes, age_depth_model_reference = input$age_depth_model_reference, age_depth_model_notes = input$age_depth_model_notes
            )
    
    generateDepthseriesTemplate()
  })
  
  ## ... 3f Confirm site metadata #########
  observeEvent(input$confirm_site_metadata, {
    generateSiteTemplate()

    updateCollapse(session, id = "site_metadata_bspanel", close = "Site Metadata")
  })
  
  ## ... 3g Confirm core metadata #########
  # core position
  observeEvent(input$confirm_core_position, {
    updateCollapse(session, id = "core_metadata_bspanel", close = "core_position_bspanel")
  })
  
  observe({
    if(input$core_position_check == TRUE){
      updateCollapse(session, id = "core_metadata_bspanel", open = "core_position_bspanel")
    } else {
      updateCollapse(session, id = "core_metadata_bspanel", close = "core_position_bspanel")
    } 
  })
  
  # core elevation
  observeEvent(input$confirm_core_elevation, {
    updateCollapse(session, id = "core_metadata_bspanel", close = "core_elevation_bspanel")
  })
  
  observe({
    if(input$core_elevation_check == TRUE){
      updateCollapse(session, id = "core_metadata_bspanel", open = "core_elevation_bspanel")
    } else {
      updateCollapse(session, id = "core_metadata_bspanel", close = "core_elevation_bspanel")
    } 
  })
  
  # confirm all core metadata 
  
  observeEvent(input$confirm_core_metadata, {
    generateCoreTemplate()
    updateCollapse(session, id = "core_metadata_bspanel", close = "Core Metadata")
  })
  
  observeEvent(input$confirm_site_core_metadata, {
    updateTabsetPanel(session, inputId = "nav", selected = "Download Templates")
  })
  
  
  ## 4. Send data to dropbox ##############
  observeEvent(input$submit_metadata, {
    uploadData()
  })
  
  uploadData <- function() {
    
    ## ... 4a Study Information ##########
    # Create a unique file name for each data table
    upload_study_information <- sprintf("%s_study_information.csv", humanTime())

    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_study_information)
    write.csv(study_information$df, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
    
    ## ... 4b Authors ##########
    # Create a unique file name for each data table
    upload_authors <- sprintf("%s_authors.csv", humanTime())
    
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_authors)
    write.csv(authors$df, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
    
    ## ... 4c Keywords ##########
    # Create a unique file name for each data table
    upload_keywords <- sprintf("%s_keywords.csv", humanTime())
    
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_keywords)
    write.csv(keywords$df, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
    
    ## ... 4d Associated Publications ########
    # Create a unique file name for each data table
    upload_associated_publications <- sprintf("%s_associated_publications.csv", humanTime())
    
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_associated_publications)
    write.csv(associated_publications$df, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
    
    ## ... 4e Methods and Materials Metadata ########
    # Create a unique file name for each data table
    upload_methods_metadata <- sprintf("%s_material_and_methods.csv", humanTime())
    
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_methods_metadata)
    write.csv(methods_metadata$df, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
  }

  ## 5. Generate Templates ###################
  
  ## ... 5a site level template ##############
  generateSiteTemplate <- function(){
    # I'm not gathering this information but may in the future: 
    site_var <- c("site_latitude_max", "site_latitude_min", "site_longitude_max", "site_longitude_min")
    
    template_var <- c("site_id", "site_description")
    
    if(input$site_salinity_check == "Yes" | input$site_salinity_check == "Only at some sites"){
      template_var <- append(template_var, c("salinity_class", "salinity_method", "salinity_notes"))
    }
    
    if(input$site_vegetation_check == "Yes" | input$site_vegetation_check == "Only at some sites"){
      template_var <- append(template_var, c("vegetation_class", "vegetation_method", "vegetation_notes"))
    }
    
    if(input$site_inundation_check == "Yes" | input$site_inundation_check == "Only at some sites"){
      template_var <- append(template_var, c("inundation_class", "inundation_method", "inundation_notes"))
    }
    
    # turn site ID UI prompt into a character vector
    site_ids <- unlist(strsplit(input$site_ids, split=";"))
    # generate template based on the number of relevant variables with the number of rows equal to the number of sites
    template_site <- data.frame(matrix(data = "", ncol=length(template_var), nrow = length(site_ids)))
    colnames(template_site) <- template_var
    # add site_ids to respective column 
    template_site$site_id <- site_ids
    
    if(input$site_salinity_check == "Yes"){
      template_site <- mutate(template_site, salinity_method = input$site_salinity_method)
    }
    
    if(input$site_vegetation_check == "Yes"){
      template_site <- mutate(template_site, vegetation_method = input$site_vegetation_method)
    }
    
    if(input$site_inundation_check == "Yes"){
      template_site <- mutate(template_site, inundation_method = input$site_inundation_method)
    }
    
    site_template$df <- template_site
  }
  
  ## ... 5b core level template ##############
  generateCoreTemplate <- function(){
    
    core_var <- c(
      "core_date", 
      "core_length_flag"
    )
    
    template_var <- c("site_id", "core_id", "core_notes")
    
    if(input$core_position_check == TRUE){
      template_var <- append(template_var, 
                             c("core_latitude", "core_longitude", "core_position_accuracy", "core_position_method", "core_position_notes"))
    }
    
    if(input$core_elevation_check == TRUE){
      template_var <- append(template_var, 
                             c("core_elevation", "core_elevation_datum", "core_elevation_accuracy", "core_elevation_method", "core_elevation_notes"))
    }
    
    if(input$core_salinity_check == "Yes" | input$core_salinity_check == "Only at some cores"){
      template_var <- append(template_var, c("salinity_class", "salinity_method", "salinity_notes"))
    }
    
    if(input$core_vegetation_check == "Yes" | input$core_vegetation_check == "Only at some cores"){
      template_var <- append(template_var, c("vegetation_class", "vegetation_method", "vegetation_notes"))
    }
    
    if(input$core_inundation_check == "Yes" | input$core_inundation_check == "Only at some cores"){
      template_var <- append(template_var, c("inundation_class", "inundation_method", "inundation_notes"))
    }
    
    if(input$core_impact_class == TRUE){
      template_var <- append(template_var, "impact_class")
    }
    
    # turn core ID UI prompt into a character vector
    core_ids <- unlist(strsplit(input$core_ids, split=";"))
    # generate template based on the number of relevant variables with the number of rows equal to the number of cores
    template_core <- data.frame(matrix(data = "", ncol=length(template_var), nrow = length(core_ids)))
    colnames(template_core) <- template_var
    # add core_ids to respective column 
    template_core$core_id <- core_ids
    
    if(input$core_position_check == TRUE){
      if(input$core_position_varies == TRUE) {
        template_core <- mutate(template_core, core_position_method = input$core_position,
                                core_position_accuracy = input$core_position_accuracy)
      }
    }
    
    if(input$core_elevation_check == TRUE){
      if(input$core_elevation_varies == TRUE) {
        template_core <- mutate(template_core, core_elevation_method = input$core_elevation_method,
                                core_elevation_accuracy = input$core_elevation_accuracy,
                                core_elevation_datum = input$core_elevation_datum)
      }
    }
    
    if(input$core_salinity_check == "Yes"){
      template_core <- mutate(template_core, salinity_method = input$core_salinity_method)
    }
    
    if(input$core_vegetation_check == "Yes"){
      template_core <- mutate(template_core, vegetation_method = input$core_vegetation_method)
    }
    
    if(input$core_inundation_check == "Yes"){
      template_core <- mutate(template_core, inundation_method = input$core_inundation_method)
    }
    
    core_template$df <- template_core
    
  }
  
  ## ... 5c depthseries template #############
  generateDepthseriesTemplate <- function(){
    
    template_variables <- c("site_id", "core_id",
                            "depth_min", "depth_max", "depth_interval_notes")
    
    age_depth_tracker <- FALSE
    
    if(datatype_tracker$df$dbd == TRUE){
      template_variables <- append(template_variables, "dry_bulk_density")
    }
    
    if(datatype_tracker$df$fraction_carbon == TRUE){
      template_variables <- append(template_variables, "fraction_carbon")
    }
    
    if(is.null(input$carbon_measured_or_modeled) == FALSE){
      if(input$carbon_measured_or_modeled == "measured"){      
        template_variables <- append(template_variables, "fraction_organic_matter")
      }
    }
    
    if(datatype_tracker$df$cs137 == TRUE) {
      template_variables <- append(template_variables, c("cs137_activity", "cs137_activity_sd"))
      age_depth_tracker <- TRUE
    }
    
    if(datatype_tracker$df$pb210 == TRUE) {
      template_variables <- append(template_variables, c("total_pb210_activity", "total_pb210_activity_sd","ra226_activity", "ra226_activity_sd",
                                                         "excess_pb210_activity", "excess_pb210_activity_sd"))
      if(input$excess_pb210_exists == TRUE){
        template_variables <- append(template_variables, c("excess_pb210_activity", "excess_pb210_activity_sd"))
      }
      
      age_depth_tracker <- TRUE
    }
    
    if(datatype_tracker$df$c14 == TRUE) {
      template_variables <- append(template_variables, c("c14_age", "c14_age_sd", "c14_material", "c14_notes", "delta_c13")) 
      age_depth_tracker <- TRUE
    }
    
    if(datatype_tracker$df$be7 == TRUE) {
      template_variables <- append(template_variables, c("be7_activity", "be7_activity_sd")) 
      age_depth_tracker <- TRUE
    }
    
    if(datatype_tracker$df$am241 == TRUE) {
      template_variables <- append(template_variables, c("am241_activity", "am241_activity_sd")) 
      age_depth_tracker <- TRUE
    }
    
    if(age_depth_tracker == TRUE){
      template_variables <- append(template_variables, c("age", "age_min", "age_max", "age_sd")) 
    }
    
    if(is.null(input$other_marker) == FALSE){
      if(input$other_marker == TRUE){
        template_variables <- append(template_variables, c("marker_date", "marker_type", "marker_notes")) 
      }
    }
    
    if(is.null(input$compaction_flag) == FALSE){
      if(input$compaction_flag == "compaction_qualified" | input$compaction_flag == "compaction_quantified"){
        template_variables <- append(template_variables, c("compaction_fraction", "compaction_notes")) 
      }
    }
    
    template_depthseries <- data.frame(matrix(data = FALSE, ncol=length(template_variables), nrow = 0))
    colnames(template_depthseries) <- template_variables
    depthseries_template$df <- template_depthseries
  
  }  

## 6. Download templates and metadata ############
  output$template_download <- downloadHandler(
    filename = function() {
      paste("CCRCN_templates.zip")
    },
    
    content = function(fname){
      tmpdir <- tempdir()
      setwd(tempdir())
      
      path <- "CCRCN_depthseries_template.csv"
      fs <- path
      write.csv(depthseries_template$df, path, row.names=F)
      
      path <- "CCRCN_core_template.csv"
      fs <- c(fs, path)
      write.csv(core_template$df, path, row.names=F)
      
      path <- "CCRCN_site_template.csv"
      fs <- c(fs, path)
      write.csv(site_template$df, path, row.names=F)
      
      path <- "study_metadata.csv"
      fs <- c(fs, path)
      write.csv(study_information$df, path, row.names = FALSE, quote = TRUE)
      
      path <- "author_metadata.csv"
      fs <- c(fs, path)
      write.csv(authors$df, path, row.names = FALSE, quote = TRUE)
      
      path <- "keywords_metadata.csv"
      fs <- c(fs, path)
      write.csv(keywords$df, path, row.names = FALSE, quote = TRUE)
      
      path <- "associated_pubs_metadata.csv"
      fs <- c(fs, path)
      write.csv(associated_publications$df, path, row.names = FALSE, quote = TRUE)
      
      path <- "methods_metadata.csv"
      fs <- c(fs, path)
      write.csv(methods_metadata$df, path, row.names = FALSE, quote = TRUE)
      
      zip(zipfile=fname, files=fs)
    }
  )
  
  ## functions ################
  
  getAuthorValue <- function(variable){
    # If no author is selected to be edited, the initial value is blank
    if(nrow(edited_author$df)==0){
      value <- ""
      return(value)
    } else {
      # return the value 
      return(variable)
    }
  }
}