# MarineGEO data submission app example 
# This is the UI script for three-file version of the prototype MarineGEO data portal
# Contact: Michael Lonneman, lonnemanM@si.edu


navbarPage("MarineGEO Data Portal", id="nav",
           
           tabPanel("Welcome", 
                    
                    absolutePanel(id = "study_info_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "25%", right = "auto", bottom = "auto",
                                  width = "50%", height = "auto", style="padding:10px;",
                                  
                                  div(id = "welcome_message", 
                                      "Welcome to the prototype MarineGEO data portal! 
                                      This application allows you to submit data and metadata to the MarineGEO database.", tags$br(), tags$br(), 
                                      
                                      "New data submissions: Review our data policy and submit your data as specified by the MarineGEO data collection protocols.", 
                                      
                                      tags$br(), tags$br(), 
                                      
                                      actionButton("data_policy_intro", "Review the MarineGEO data policy and submit data.", 
                                                   class = "btn-primary"), tags$br(), tags$br() 
                                
                                  )
                    )
           ),
           
           tabPanel("Data Policy",
                    
                    absolutePanel(
                      id = "memorandum", class = "panel panel-default", fixed = TRUE,
                      draggable = FALSE, top = "auto", left = "auto", right = "auto", bottom = "auto",
                      width = "70%", height = "90%", style="padding: 50px; overflow-y: scroll;",
                      
                      tags$div(
                        tags$h4(tags$b("Memorandum of CCRCN Data Responsibility"), align="center"), tags$br(),
                        
                        "The Coastal Carbon Research Coordination Network (", 
                        tags$a(href="https://www.nsf.gov/awardsearch/showAward?AWD_ID=1655622&HistoricalAwards=false", "NSF 1655622"),
                        ", herein CCRCN) is an initiative
                        to accelerate the pace of discovery in coastal wetland science by providing the community with access to data, 
                        open source analysis tools, and data synthesis opportunities. A major component of these efforts is to network 
                        and disseminate previously-collected data, both for CCRCN research efforts and for use by the community. 
                        Many of such data are previously published, or otherwise publically available for scientific exploration 
                        (herein public data). The CCRCN also seeks to assist users with data that are not yet public 
                        (herein private data) to participate, and be properly acknowledged in, future data syntheses, through the 
                        creation of downloadable and citable public data releases primarily attributable to the original data contributor(s).
                        In all cases, we respect the intellectual property rights of data producers." ,
                        
                        tags$br(),
                        
                        "This means:", 
                        tags$br(), tags$br(),
                        tags$ul(
                          tags$li("Proper citations and/or contact information are associated with each data point."), 
                          tags$li("Citations to all publications referenced within CCRCN synthetic datasets are be 
                                  posted to our public websites and are included in dataset downloads."), 
                          tags$li("Private data will not be redistributed without explicit consent of the data owner."),
                          tags$li("CCRCN personnel defer to data contributors on when and if private data are to be made public.")
                          ),
                        
                        "The CCRCN does not claim ownership of the datasets we curate. Instead we assist data submitters in 
                        formatting data releases, securing space on a public data repository, and issuing a citable digital 
                        object identifiers (DOI). CCRCN personnel are trained to create stable, machine readable, analysis-ready 
                        datasets based on community generated standards. Each data release includes detailed metadata to ensure 
                        that the context and purposes of a contributor’s study are represented with an appropriate level of detail.",
                        
                        tags$br(),tags$br(),
                        
                        "Finally, CCRCN compiles publically available data into a central data clearing house. 
                        These include both data releases curated by the CCRCN and those originating from other public sources. 
                        Datasets are downloaded, reformatted to a common standard, and compiled with other studies using publically 
                        available open source R scripts. These scripts and compiled data files are available on a public GitHub 
                        repository, and accessible through the Coastal Carbon Atlas map interface. The publically available 
                        GitHub is completely separate from our data curation work flows, so there is no risk for accidental 
                        public release of private data. The CCRCN provides associated bibliographies to assist users 
                        in citing original data sources, but users are ultimately responsible for correctly citing all data used.",
                        
                        tags$br(), tags$br(), 
                        
                        "You must acknowledge and accept the data policy in order to submit data.", tags$br(), tags$br(),
                        
                        actionButton("new_submission", "I accept the MarineGEO data policy", 
                                     class = "btn-primary"), tags$br(), tags$br(),
                        
                        actionButton("return_to_intro", "Return to previous page"), tags$br(), tags$br()
                        
                      )
                    )
                  ),
           
           tabPanel("Data Upload", 
                    
                    absolutePanel(id = "study_info_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "25%", right = "auto", bottom = "auto",
                                  width = "50%", height = "auto", style="padding:10px;",
                                  
                                  div(id = "file_uploads",
                                      
                                      "Please enter your email address (required to submit data).", 
                                      tags$br(), 
                                      
                                      textInput("email", label = NULL, width = "80%"),
                                      
                                      fileInput("fileCSV", "Upload CSV File",
                                                multiple = TRUE,
                                                accept = c("text/csv",
                                                           "text/comma-separated-values,text/plain",
                                                           ".csv")),
                                      
                                      # Horizontal line ----
                                      tags$hr(),
                                      
                                      # Input: Checkbox if file has header ----
                                      checkboxInput("header", "Header", TRUE),
                                      
                                      # Input: Select separator ----
                                      radioButtons("sep", "Separator",
                                                   choices = c(Comma = ",",
                                                               Semicolon = ";",
                                                               Tab = "\t"),
                                                   selected = ","),
                                      
                                      tags$br(), tags$br(), 
                                      
                                      # Submit button starts as disabled until the user provides an email address 
                                      actionButton("submit", "Submit", class = "btn-primary"), tags$br(), tags$br(), 
                                      
                                      actionButton("return_to_data_policy", "Return to the data policy page"), tags$br(), tags$br()
                                      
                                  )
                    )
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
                    )
           ),
           
           # Users cannot user the header to move between tabs so the cursor will need to change accordingly
           useShinyjs(), 
           tags$head(tags$style(HTML('.navbar-nav a {cursor: default}')))
           
)
