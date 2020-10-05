# MarineGEO data submission app example 
# This is the UI script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu


bootstrapPage('', 
navbarPage("MarineGEO Data Portal", id="nav", 

           tabPanel("Welcome", 
                    
                    fluidPage(
                      column(width = 10,
                                  
                                  div(img(src="Logomark_MarineGEO_Tennenbaum_RGB.png", width=250)),
                                  
                                  div(id = "welcome_message", 
                                      tags$br(), tags$br(), 

                                      "Welcome to the MarineGEO data portal! 
                                      This application allows you to submit data to MarineGEO", tags$br(), tags$br(), 
                                      
                                      tags$b("Seagrass Surveys"), tags$br(),
                                      "Review our data policy and submit your data as specified by the MarineGEO data collection protocols. ", 
                                      "You must upload your data on our MarineGEO spreadsheets, which you can find on our", 
                                      
                                      tags$a(href="https://marinegeo.github.io/modules.html", "website."),
                                      
                                      "Each file must be an Excel file (the file extension is '.xlsx'). ",
                                      "We do not require your files to have a specific naming convention.",
                                      
                                      tags$br(), tags$br(), 
                                      
                                      updateActivePageUI("welcome_survey_data",
                                                         "Submit survey data.",
                                                         "btn-primary"), tags$br(), tags$br(), 
                                      
                                      tags$b("Sonde Deployments"), tags$br(),
                                      
                                      "Review our data policy, provide information about the sonde deployment, 
                                      and upload your data in the standardized korexo CSV format.", tags$br(), tags$br(),
                                      
                                      updateActivePageUI("welcome_sensor_data",
                                                            "Submit sensor data",
                                                            "btn-primary")
                                      
                                  )
                    ),
                    
                    # See global for footer code 
                    footer 
                    )
           ),
           
           ## Data policy and authorship agreement ####
           tabPanel("Data Policy",
                    
                    fluidPage(
                      column(width = 10, 
                             div(
                               tags$iframe(style="height:600px; width:100%", 
                                           src="MarineGEO Data Policy Authorship Agreement.pdf"),
                               
                               updateActivePageUI("new_submission", "I accept the MarineGEO data policy",
                                                     "btn-primary"), tags$br(), tags$br(),
                               
                               updateActivePageUI("return_to_intro", "Return to previous page", NA)
                             )
                             
                      ),
                      # See global for footer code 
                      footer 
                    )
           ),
           
           ## Data Upload ############
           tabPanel("Data Upload", 
                    
                    fluidPage(
                      column(width = 10,
                             
                             div(
                               img(src="Logomark_MarineGEO_Tennenbaum_RGB.png", width=250), 
                               tags$br(), tags$br()
                             ),
                             
                             div(id = "file_uploads",
                                 uiOutput("file_upload"), 
                                 tags$br(), tags$br(),
                                 updateActivePageUI("return_to_data_policy", "Return to previous page", NA)
                             )
                      ),
                      # See global for footer code 
                      footer 
                    )
           ), 
           
           ## Data Report ####
           tabPanel("Data Report", 
                    
                    absolutePanel(id = "study_info_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "auto", right = "auto", bottom = "auto",
                                  width = "90%", height = "auto", style="padding:10px;",
                                  
                                  div(id = "file_uploads",
                                      
                                      uiOutput("html_report"),

                                      tags$br(), tags$br(),
                                      
                                      downloadButton("downloadReport", "Download report"),

                                      tags$br(), tags$br()
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

