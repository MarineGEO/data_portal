# MarineGEO data submission app example 
# This is the server script for three-file version of an example data submission application
# Contact: Michael Lonneman, lonnemanM@si.edu

function(input, output, session) {
  
  # Action button logic
  observeEvent(input$data_policy_intro, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Policy")
  })

  observeEvent( input$new_submission, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Upload")
  })
  
  observeEvent( input$submit, {
    updateTabsetPanel(session, inputId = "nav", selected = "Data Report")
  })
  
}