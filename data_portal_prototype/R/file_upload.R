fileUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    output$file_review <- renderTable({
      if(!is.null(input$upload)){
        input$upload %>%
          select(name) %>%
          rename(`Uploaded Files` = name)
      } else {
        tibble(`Uploaded Files` = "No files uploaded")
      }
    })
    
    # data <- reactive(mtcars[[input$var]])
    # output$hist <- renderPlot({
    #   hist(data(), breaks = input$bins, main = input$var)
    # }, res = 96)
    
    
  })
}


# checkFileExtensions <- function(){
#   
#   if(any(!grepl("xlsx", input$fileExcel$name))){
#     # Update protocol error message with proper warning
#     protocol_error_message(filter(warnings, title == "no_excel_files")$message)
#     return(FALSE)
#     
#   } else return(TRUE)
# }

# @param id character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
# @param multiple_allowed Logical, are multiple files allowed to be uploaded
# @param extension file extension that fileInput accepts 

fileUploadUI <- function(id, multiple_allowed, extension) {
  tagList(
    "Please enter your email address (required to submit data).",
    tags$br(), tags$br(),
    textInput(NS(id, "email"), label = NULL),
    hr(), #tags$br(), tags$br(),
    if(multiple_allowed){
      div(
        "You can upload multiple files by either clicking \"Browse\" once and selecting multiple files or",
        " by dragging multiple files simultaneously into the box below. If you browse to select or drag files more than once,",
        " previous files will be overwritten. Check the list of \"Uploaded Files\" to ensure that each file",
        " has been uploaded before clicking \"Submit\"."
      )
    } else {
      div(
        "Upload a single file. ",
        "Check the filename under \"Uploaded Files\" to ensure that the correct file",
        " has been uploaded before clicking \"Submit\"."
      )
    },
    fileInput(NS(id, "upload"), "",
              multiple = multiple_allowed,
              accept = extension),
    hr(),
    tableOutput(NS(id,"file_review")),
    actionButton(NS(id, "submit"), "Submit", class = "btn-primary")
  )
}