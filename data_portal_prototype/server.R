# MarineGEO data submission app example 
# This is the server script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu

function(input, output, session) {
  
  ## Action button logic ###############
  # Move to data policy page from introduction
  observeEvent(input$data_policy_intro, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Policy")
  })
  # Accept data policy and move to data submission
  observeEvent( input$new_submission, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Upload")
  })
  # Return to welcome screen from data policy 
  # Does not indicate agreement with data policy
  observeEvent(input$return_to_intro, {
    updateTabsetPanel(session, inputId = "nav", selected = "Welcome")
  })
  # Confirm data submission
  # As long as the user has also provided an excel file and an email address, the initial file will be uploaded to a dropbox directory
  # QA/QC tests will be conducted on the file and a RMD report generated 
  # The user will be moved to a page with the report and whether their submission was successful
  observeEvent( input$submit, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Report")
  })
  # Return to data policy from submission page
  observeEvent(input$return_to_data_policy, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Policy")
  })
  # Return to data submission page 
  observeEvent(input$return_to_upload, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Upload")
  })
  
  # Prevent users from clicking on tabs in the header 
  # They'll need to use action buttons to move between pages
  shinyjs::disable(selector = '.navbar-nav a')
  
}