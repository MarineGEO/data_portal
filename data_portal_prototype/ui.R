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

                                      "Welcome to the MarineGEO data portal! 
                                      This application allows you to submit data and metadata to the MarineGEO database.", tags$br(), tags$br(), 
                                      
                                      "New data submissions: Review our data policy and submit your data as specified by the MarineGEO data collection protocols. ", 
                                      "You must upload your data on our MarineGEO spreadsheets, which you can find on our", 
                                      
                                      tags$a(href="https://marinegeo.github.io/modules.html", "website."),
                                      
                                      "Each file must be an Excel file (the file extension is '.xlsx'). ",
                                      "We do not require your files to have a specific naming convention.",
                                      
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
                        
                        img(src="Logomark_MarineGEO_Tennenbaum_RGB.png", width=250),

                        tags$br(), tags$br(),

                        tags$h3(tags$b("MarineGEO Data Policy & Authorship Agreement"), align="center"),
                        tags$h4("v0.1.0", align="center"), tags$br(),

                        tags$h4(tags$b("Summary")),

                        tags$ul(
                          tags$li("Most data are made available to all participants in the network immediately after basic quality control."),
                          tags$li("Data are typically made public within 2 years of submission."),
                          tags$li("Use of data prior to its public release requires pre-approval by the MarineGEO Central Team and proper attribution on any scientific products."),
                          tags$li("Participants who make substantial contributions to MarineGEO activities will be eligible for co-authorship on resulting publications and products.")
                        ),
                        
                        hr(), 

                        tags$h4(tags$b("Overview")),
                        "MarineGEO values collaboration and open sharing of our collective knowledge.
                        Acquiring, curating, sharing, and analyzing data are central functions of our network’s research.
                        This document summarizes guidelines for data contribution, sharing, and authorship by contributors to activities
                        coordinated by the Smithsonian-led MarineGEO program.
                        Adherence to the data policy described here is a requirement for any use of MarineGEO data
                        or participation in any MarineGEO activity that involves data collection.",tags$br(), tags$br(), 

                        tags$h4(tags$b("Terms & Definitions")),
                        tags$em("MarineGEO"), " refers to both the Marine Global Earth Observatory research program and the broader community
                        (network) of partners that conduct its activities, which are led and coordinated by the MarineGEO Central
                        Team at the Smithsonian Institution.", tags$br(), tags$br(), 

                        tags$em("MarineGEO Central"), " refers to the leadership and support team, based at the Smithsonian,
                        that coordinates MarineGEO activities. MarineGEO Central is responsible for collating, curating,
                        archiving, and serving MarineGEO data to contributing participants and, where appropriate, to the public.", tags$br(), tags$br(), 

                        tags$em("Data")," refers to all content generated by MarineGEO activities,
                        including measurements stored in spreadsheets and on original field sheets,
                        images, video, genetic sequences, GIS layers, computer code, and associated metadata.", tags$br(), tags$br(), 

                        "A ", tags$em("contributing participant"), " is any party who has collected and contributed original data to MarineGEO.
                        Participants can include principal investigators, technicians, students, and/or volunteers at
                        MarineGEO partner institutions, as well as those contributing to a MarineGEO project by mutual agreement.",tags$br(), tags$br(), 

                        tags$h4(tags$b("Data Collection")),

                        "MarineGEO provides standard protocols and data formats for most network activities.
                        Contributing participants are responsible for ensuring the data they collect adheres
                        to MarineGEO best practices. Contributing participants agree to utilize these
                        resources provided by MarineGEO, including protocols, field sheets, and data entry spreadsheets.",tags$br(), tags$br(), 

                        tags$h4(tags$b("Data Submission")),
                        "Contributing participants are responsible for using MarineGEO’s data submission guidelines and
                        portal to submit their data for curation in a timely fashion (usually within six months of
                        collection unless otherwise noted). Contributors also are responsible for responding to any
                        requests that arise during the Quality Assurance--Quality Control (QAQC) process to ensure
                        data quality and completeness.",tags$br(), tags$br(), 

                        tags$h4(tags$b("Data Availability")),
                        "MarineGEO strives toward open-access data. However, we recognize there are different categories of data
                        (Table 1) and contributors deserve priority in their capacity to utilize certain types of data they contribute.
                        In general, data collected as part of MarineGEO activities will be made available to contributing participants
                        immediately upon meeting all requirements for data quality. In addition, participants
                        will have access to all data relevant to the project(s) in which they participate.
                        Certain types of data, including products of experiments and student thesis research,
                        can be embargoed for a term specified by mutual agreement before public release.  ", tags$br(), tags$br(), 

                        "Metadata associated with MarineGEO data will be made available to the public along with the data,
                        except for certain types of sensitive data such as locations of vulnerable species or habitats,
                        and Personally Identifiable Information (PII), including names, contact information, and images of people.
                        If you do not wish for such information to be made public, please indicate so during data submission. ", tags$br(), tags$br(), 

                        "Approximately two years after the initial submission, MarineGEO intends to make all applicable data available on public
                        repositories with proper attribution (see Table 1 and Data Attribution).", tags$br(), tags$br(), 

                        tags$h4(tags$b("Data Sharing")),
                        "Contributing participants own the raw data they collect.", tags$br(), tags$br(), 
                        "MarineGEO may analyze, synthesize, and share data as needed to advance the MarineGEO community’s
                        collective goals, provided this does not conflict with other guidelines described here.
                        Data may be shared with external parties prior to public release to assist in data curation,
                        analysis, and/or synthesis as needed to ensure quality and accessibility at the
                        discretion of MarineGEO Central Team.", tags$br(), tags$br(), 
                        "Sharing data collected by another MarineGEO partner with parties outside the network prior to public release
                        requires the prior written approval of the contributing participant(s) who generated the data and the MarineGEO Central Team.",tags$br(), tags$br(), 

                        tags$h4(tags$b("Data Use")),
                        "Contributing participants are free to use any MarineGEO data to which they have access for scientific purposes (e.g., manuscripts, dissertations, theses) 
                        within the guidelines described in this document, or unless expressly restricted by agreement for a particular use. ", tags$br(), tags$br(), 
                        
                        "Where data are collected for a particular use (e.g., a collaborative MarineGEO project), those data may not be shared outside the network or published 
                        until the collaborative project is concluded, unless the project participants and MarineGEO Central Team grant prior approval. ", tags$br(), tags$br(), 
                        
                        "Data may not be used for grant or contract proposals, or for commercial or for-profit applications, without the prior written 
                        approval of MarineGEO.", tags$br(), tags$br(), 

                        tags$h4(tags$b("Data Attribution and Publication")),
                        "All products developed using data collected for and/or provided by MarineGEO must provide proper attribution in the form of a written acknowledgment citing MarineGEO 
                        and any contributing partners as the source of the data. Where available, 
                        these products must also cite the data set’s Digital Object Identifier (DOI).", tags$br(), tags$br(), 
                        "Authors of peer-reviewed publications and other products using MarineGEO data must request and register a MarineGEO contribution number prior to 
                        publication and cite it in the acknowledgements of the publication (see: https://marinegeo.si.edu/publication-request-form). ", tags$br(), tags$br(), 

                        tags$h4(tags$b("Data Updates")),
                        "Data may be updated periodically to fix errors, update nomenclature, and adhere to current standards. MarineGEO will strive to make the identity of the 
                        latest version and the history of all data sets clear.  It is the user’s responsibility to ensure that they are working with the most current version.", 
                        tags$br(), tags$br(), 
                        
                        tags$h4(tags$b("Data Liability")),

                        "All data provided by MarineGEO are made available “as is.” It is the sole responsibility of the user to ensure that the data 
                        are being used and interpreted properly, and that they adhere to the terms of this agreement. 
                        Neither the contributing participant(s) nor the Smithsonian Institution are liable for any damages resulting from any use or 
                        interpretation of the data.", tags$br(), tags$br(), 
                        
                        tags$h4(tags$b("Authorship")),
                        "The opportunity for co-authorship on publications and products resulting from MarineGEO activities will be extended to contributing participants 
                        according to typical norms of scientific societies and journals. Generally, co-authors shall meet both of the following qualifications:", tags$br(), tags$br(), 
                        
                        tags$ul(
                          tags$li("Submit quality data that is used in the publication; and"),
                          tags$li("Provide substantial input to lead author(s) during preparation of the manuscript.")
                        ),
                        
                        "Contributing participants who provide additional services, such as submitting a relatively large amount of data, conducting key analyses, 
                        and/or contributing disproportionately to writing and manuscript preparation, may be recognized with priority ordering in the author list. 
                        Otherwise, co-authors should be listed alphabetically.", tags$br(), tags$br(),
                        
                        "It is the joint responsibility of the lead author of a manuscript and the Principal Investigator of each contributing group to ensure all 
                        individuals who meet criterion #1 are identified early in manuscript preparation and offered the opportunity to meet criterion #2 for co-authorship.",
                        
                        tags$br(), tags$br(),
                        
                        tags$b("Table 1."), " Description of data types and their availability to contributing participants (the network) and their public release",
                        tags$br(),
                        
                        tableOutput("data_policy"),
                        
                        "* No use in individual manuscripts before network manuscript is accepted, unless approved by MarineGEO Central Team", tags$br(),
                        "** Smithsonian Institution (SI) requires that non-SI individuals sign a waiver before appearing in images displayed by SI to the public",
                        
                        hr(), 

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
                                      
                                      "Upload official MarineGEO data spreadsheets below. You can find these spreadsheets on our",
                                      tags$a(href="https://marinegeo.github.io/modules.html", "website."), tags$br(), tags$br(),
                                      
                                      "You can upload multiple spreadsheets by either clicking \"Browse\" once and selecting multiple files or",
                                      " by dragging multiple files simultaneously into the box below. If you browse to select or drag files more than once,",
                                      " previous files will be overwritten. Check the list of \"Uploaded Files\" to ensure that each file",
                                      " has been uploaded before clicking \"Submit\".",
                                      
                                      fileInput("fileExcel", "",
                                                multiple = TRUE,
                                                accept = c(".xlsx")),
                                      hr(), 
                                      
                                      tableOutput('uploaded'),
                                      
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

