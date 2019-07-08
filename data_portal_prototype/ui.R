# MarineGEO data submission app example 
# This is the UI script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu


bootstrapPage('', 
navbarPage("MarineGEO Data Portal", id="nav", 

           tabPanel("Welcome", 
                    
                    absolutePanel(id = "study_info_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "25%", right = "auto", bottom = "auto",
                                  width = "50%", height = "auto", style="padding:10px;",
                                  
                                  div(img(src="Logomark_MarineGEO_Tennenbaum_RGB.png", width=250)),
                                  
                                  div(id = "welcome_message", 
                                      tags$br(), tags$br(), 

                                      "Welcome to the prototype MarineGEO data portal! 
                                      This application allows you to submit data and metadata to the MarineGEO database.", tags$br(), tags$br(), 
                                      
                                      "New data submissions: Review our data policy and submit your data as specified by the MarineGEO data collection protocols.", 
                                      
                                      tags$br(), tags$br(), 
                                      
                                      actionButton("data_policy_intro", "Review the MarineGEO data policy and submit data.", 
                                                   class = "btn-primary"), tags$br(), tags$br() 
                                
                                  )
                    ),
                    
                    # See global for footer code 
                    footer 
           ),
           
           ## Data policy ############
           tabPanel("Data Policy",
                    
                    absolutePanel(
                      id = "memorandum", class = "panel panel-default", fixed = TRUE,
                      draggable = FALSE, top = "auto", left = "auto", right = "auto", bottom = "auto",
                      width = "70%", height = "90%", style="padding: 50px; overflow-y: scroll;",
                      
                      div(
                        img(src="Logomark_MarineGEO_Tennenbaum_RGB.png", width=250), 
                          
                        tags$br(), tags$br()
                      ),
                      
                      tags$div(
                        tags$h4(tags$b("Data Policy & Authorship Agreement"), align="center"), tags$br(),
                        
                        "Data collected as part of MarineGEO activities will be made available to contributing participants 
                        immediately upon meeting all requirements for data quality assurance. 
                        Contributors indicated during data submission will have access to all data 
                        relevant to the project(s) in which they are participating at the time. ", 
                        
                        tags$br(), tags$br(), 
                        
                        "Data will contain identifying information, such as the specific geographic location and taxonomic 
                        identities, and names and contact information of the collectors, including institutional affiliation 
                        and e-mail addresses. If you do not wish this information to be made public and/or it must be 
                        anonymized, please indicate so during data submission.",
                        
                        tags$br(), tags$br(), 
                        
                        "You must acknowledge and accept the data policy in order to submit data.", tags$br(), tags$br(),
                        
                        actionButton("new_submission", "I accept the MarineGEO data policy", 
                                     class = "btn-primary"), tags$br(), tags$br(),
                        
                        actionButton("return_to_intro", "Return to previous page"), tags$br(), tags$br()
                        
                      )
                    ),
                    
                    # See global for footer code 
                    footer 
                  ),
           
           ## Data Upload ############
           tabPanel("Data Upload", 
                    
                    absolutePanel(id = "study_info_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "25%", right = "auto", bottom = "auto",
                                  width = "50%", height = "auto", style="padding:10px;",
                                  
                                  div(
                                    img(src="Logomark_MarineGEO_Tennenbaum_RGB.png", width=250), 
                                    
                                    tags$br(), tags$br()
                                  ),
                                  
                                  div(id = "file_uploads",
                                      
                                      tags$br(), 
                                      
                                      "Please enter your email address (required to submit data).", 
                                      tags$br(), tags$br(), 
                                      
                                      textInput("email", label = NULL, width = "80%"),
                                      hr(), 
                                      
                                      "Does your data include sensitive information, such as the location of endangered animals or personally identifiable information?",
                                      tags$br(), tags$br(),
                                      
                                      selectInput("sensitive_prompt", label = NULL, choices = c("Not specified", 
                                                                                                "Yes, my data contains sensitive information", 
                                                                                                "No, my data does not contain sensitive information")), 
                                      hr(),
                                      
                                      fileInput("fileExcel", "Upload Excel files",
                                                multiple = TRUE,
                                                accept = c(".xlsx")),
                                      hr(), 
                                      
                                      tags$br(),
                                      
                                      # Submit button starts as disabled until the user provides an email address 
                                      actionButton("submit", "Submit", class = "btn-primary"), tags$br(), tags$br(), 
                                      
                                      actionButton("return_to_data_policy", "Return to the data policy page"), 
                                      
                                      tags$br(), tags$br()
                                      
                                  )
                    ),
                    
                    # See global for footer code 
                    footer 
           ), 
           
           tabPanel("Data Report", 
                    
                    absolutePanel(id = "study_info_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "25%", right = "auto", bottom = "auto",
                                  width = "50%", height = "auto", style="padding:10px;",
                                  
                                  div(id = "file_uploads",
                                      "This page will display QA/QC results based on the data submitted on the previous page. If a data submission passes QA/QC, 
                                      it will be uploaded to MarineGEO. If it fails, the results will be displayed to the uploader, and they will need to make 
                                      corrections before attempting to upload their data again.", 
                                      
                                      tags$br(), tags$br(),
                                      
                                      actionButton("return_to_upload", "Return to the data upload page"), tags$br(), tags$br()
                                  )
                    ),
                    
                    # See global for footer code 
                    footer 
           ),
           
            useShinyjs()

), 

tags$style(type = 'text/css', 

           '.navbar { background-color: #97C8EB;
           font-family: Arial;
           font-size: 13px;
           color: #FF0000; }',
           
           '.navbar-dropdown { background-color: #97C8EB;
           font-family: Arial;
           font-size: 13px;
           color: #FF0000; }',
           
           '.navbar-default .navbar-brand {
           color: #2a2c2d;
           }',
           
           '.nav.navbar-nav li a {
             color: #2a2c2d;
           }',
           
           # Users cannot use the header to move between tabs so the cursor will need to change accordingly
           '.navbar-nav a {cursor: default}'
           
)

)

