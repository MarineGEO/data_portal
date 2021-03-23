library(shiny)
library(DT)
library(readxl)

ui <- pageWithSidebar(
  headerPanel(""),
  
  sidebarPanel(
    
    div(img(src="Logomark_MarineGEO_Tennenbaum_RGB.png", width=250)),
    
    tags$br(), tags$h4("Data Submission Portal Placeholder Application"), 
    
    tags$br(), tags$br(), 
    
    fileInput("fileExcel", "Upload your Excel file",
              multiple = FALSE,
              accept = c(".xlsx")),
    
    actionButton("submit", "Submit uploaded data")
  ),
  
  mainPanel(
    tabsetPanel(
      id = "uploaded_data_table",
      tabPanel("tab_df1", dataTableOutput("df1")),
      tabPanel("tab_df2", dataTableOutput("df2"))
    )
  )
)

server <- function(input, output) {
  
  uploaded_data <- reactiveValues(df1 = data.frame(),
                                  df2 = data.frame())
  
  output$df1 <- renderDataTable({
    datatable(uploaded_data$df1)
  })
  
  output$df2 <- renderDataTable({
    datatable(uploaded_data$df2)
  })
  
  observeEvent(input$submit, {
    
    file_test <- checkFileExtensions()
    
    if(file_test){
    
      available_sheets <- excel_sheets(input$fileExcel$datapath[1])
      
      uploaded_data$df1 <- read_excel(input$fileExcel$datapath[1], sheet = 1)
      
      if(length(available_sheets) > 1){
        uploaded_data$df2 <- read_excel(input$fileExcel$datapath[1], sheet = 2)
      }
    }
  })
  
  checkFileExtensions <- function(){
    
    if(any(!grepl("xlsx", input$fileExcel$name))){
      
      showModal(modalDialog(
        title = "Failed Submission", 
        div("The uploaded file is not an Excel workbook. Check that the file extension is '.xlsx'."),
        
        easyClose = TRUE
      ))
      return(FALSE)
      
    } else return(TRUE)
  }
  
}

shinyApp(ui, server)