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
                      width = "90%", height = "90%", style="padding: 50px;overflow-y: scroll;",
                      
                      div(
                      #   img(src="Logomark_MarineGEO_Tennenbaum_RGB.png", width=250), 
                      #     
                      #   tags$br(), tags$br()
                      # ),
                      # 
                      # tags$div(
                      #   tags$h4(tags$b("Data Policy & Authorship Agreement"), align="center"), tags$br(),
                      #   
                      #   tags$a(href="https://marinegeo.github.io/assets/projects/seagrass-food-webs//MarineGEO_Data_Policy_Authorship_Agreement_v0.0.2.pdf",
                      #          "Review the entire MarineGEO data policy and authorship agreement here."),
                      # 
                      #   tags$br(), tags$br(), 
                      #   
                      #   tags$h5("Summary"), 
                      #   
                      #   tags$ul(
                      #     tags$li("Data are made available to all participants in the network immediately"),
                      #     tags$li("Data are made public within (2) years of submission"),
                      #     tags$li("Use of data for any scientific purpose requires proper attribution"), 
                      #     tags$li("Co-authorship on any MarineGEO-lead products is open and flexible but contingent
                      #              on providing substantial contributions (e.g., data, analysis, writing), and must
                      #              be extended to contributing participants by lead authors on other products")
                      #   ),
                      #   
                      #   "You must acknowledge and accept the data policy in order to submit data.", tags$br(), tags$br(),
                      #   
                      #   actionButton("new_submission", "I accept the MarineGEO data policy", 
                      #                class = "btn-primary"), tags$br(), tags$br(),
                      #   
                      #   actionButton("return_to_intro", "Return to previous page"), tags$br(), tags$br()
                      #   
                      # )
                      tags$iframe(style="height:600px; width:100%", 
                                  src="MarineGEO_Data_Policy_Authorship_Agreement_v0.0.2.pdf"), tags$br(), tags$br(),
                      
                        "You must acknowledge and accept the data policy in order to submit data.", tags$br(), tags$br(),

                        actionButton("new_submission", "I accept the MarineGEO data policy",
                                     class = "btn-primary"), tags$br(), tags$br(),

                        actionButton("return_to_intro", "Return to previous page")
                      
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
                                      
                                      "Please enter your email address (required to submit data). 
                                      You can include multiple emails, each separated by a semicolon (;).", 
                                      
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
                                      uiOutput("html_report"),

                                      tags$br(), tags$br(),
                                      
                                      downloadButton("downloadReport", "Download report"),

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

