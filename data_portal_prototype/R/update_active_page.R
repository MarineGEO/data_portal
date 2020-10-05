# Update active tabset page based on actionButton input. By default the function returns
# a reactive Value. To use it, assign the server module call to an object. The value returned
# is the ID. 
#
# @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
# @param input_id The ID of the navbarPage or similar construct
# @param new_page The name of the tabPanel page to change to
# @param parent_session Session for the parent container

updateActivePageServer <- function(id, input_id, new_page, parent_session){
  moduleServer(id, function(input, output, session) {
    
    # ReactiveValue to return
    button_return_value <- reactiveVal(NA)
    
    observeEvent(input$button, {
      updateTabsetPanel(session = parent_session, inputId = input_id, selected = new_page)
      
      button_return_value(id)
    })
    
    return(button_return_value)
    
  })
}

# Module UI to create action button tied to updating active page

# @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
# @param label text label
# @param button_class CSS class to customize button appearance 

updateActivePageUI <- function(id, label, button_class){
  actionButton(NS(id, "button"), label, class = button_class)
}
