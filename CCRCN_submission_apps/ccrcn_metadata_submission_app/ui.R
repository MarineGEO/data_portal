# Coastal Carbon Research Coordination Network
# This is the UI script for three-file version of the CCRCN data submission application
# Contact: Michael Lonneman, lonnemanM@si.edu
#          Dave Klinges, klingesD@si.edu


## 1 Introduction ######################################
# The UI interface is designed to effectively gather study metadata
# and generate detailed templates to faciliate data submissions. 
# Users will be asked broad questions about their data and more specific 
# queries will be generated based on their answers to the first set 
# of questions. 

# Additionally, study metadata will be used to create custom CSV templates 
# that can be downloaded and filled out. 

navbarPage("Coastal Carbon Data Submission Application", id="nav",
           
           tabPanel("Welcome", 
                    
                    absolutePanel(id = "study_info_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "25%", right = "auto", bottom = "auto",
                                  width = "50%", height = "auto", style="padding:10px;",
                                  
                                  div(id = "welcome_message", 
                                      "Welcome to the Coastal Carbon Research Coordination Network's data submission application! 
                                      This application allows you to submit data and metadata for inclusion into the coastal carbon 
                                      data clearinghouse and to revise existing metadata in the clearinghouse.", tags$br(), tags$br(), 
                                      
                                      "New data submissions: Tell us about your data and we will generate a custom set of templates. 
                                      There is currently no feature to save incomplete submissions. 
                                      Please complete all relevant sections before leaving the submission dashboard.", tags$br(), tags$br(), 
                                      
                                      actionButton("new_submission", "Submit new data to the CCRCN data clearinghouse", 
                                                   class = "btn-primary"), tags$br(), tags$br(), 
                                      
                                      actionLink("data_policy_intro", "Review the CCRCN's data policy.")
                      
                                      
                                      # "Revise existing metadata: We need help completing metadata for the data already stored in our 
                                      # clearinghouse. This includes information on authors, associated publications, and methods and 
                                      # materials. Click the button below, and select a study to begin.", tags$br(), tags$br(), 
                                      # 
                                      # actionButton("edit_metadata", "Edit metadata for data in the CCRCN clearinghouse", 
                                      #              class = "btn-primary"), tags$br(), tags$br(),
                                      # 
                                      # uiOutput("edit_study")
                                      )
                    )
           ),
                    
           ## 2 Study Metadata ####################
           tabPanel("Study Information",
                    
                    shinyjs::useShinyjs(),
                    
                    absolutePanel(id = "study_info_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "3%", right = "auto", bottom = "auto",
                                  width = "94%", height = "auto", style="padding:10px;",
                                  
                                  div(id = "study_info_introduction", 
                                      tags$h3("Study Information"), 
                                      "Enter metadata associated with your study, including authors, associated publications, and keywords.
                                  This information is essential for creating Ecological Metadata Language (EML), and will ensure your data
                                  is easily discoverable, you are correctly cited when your data is used, and
                                  that the history, context, and originality of your research is clear to others", 
                                      tags$br(), tags$br()
                                  ),
                                  
                                  bsCollapse(id = "study_information_bspanel",
                                             
                                             ## ... 2a Study Information ####################
                                             # Title, one-liner, absract, start and end date
                                             bsCollapsePanel(           
                                               title = "Study Information",
                                               div(
                                                 id = "study_info_table",
                                                 
                                                 uiOutput("title"),
                                                 uiOutput("one_liner"), 
                                                 uiOutput("abstract"), 
                                                 uiOutput("start_date"),
                                                 uiOutput("end_date"),
                                                 
                                                 actionButton("add_study_information", "Confirm study information", class = "btn-primary")
                                               )
                                             )
                                  ),
                                  bsCollapse(id = "authors_bspanel",
                                             
                                             ## ... 2b Authors #################################
                                             bsCollapsePanel(title="Authors",           
                                                             
                                                             div(
                                                               id = "authors_table",
                                                               
                                                               uiOutput("last_name"),
                                                               uiOutput("given_name"),
                                                               uiOutput("institution"),
                                                               uiOutput("email"),
                                                               uiOutput("address"),
                                                               uiOutput("phone"),
                                                               uiOutput("corresponding_author"),
                                                               
                                                               uiOutput("add_author"), tags$br(), 
                                                               uiOutput("add_last_author"),
                                                               # The following action button will only appear if in edit mode
                                                               uiOutput("confirm_author_edits"),
                                                               
                                                               tags$br(), tags$br(), 
                                                               
                                                               uiOutput("author_list"), 
                                                               uiOutput("edit_author")
                                                            
                                                             ))),
                                  
                                  
                                  ## ... 2c Keywords ###########
                                  
                                  bsCollapse(id = "keywords_bspanel",
                                             
                                             bsCollapsePanel(title="Keywords",           
                                                             
                                                             div(id = "keywords_table",
                                                                 uiOutput("keywords"),
                                                                 
                                                                 actionButton("add_keywords", "Submit keywords", 
                                                                              class = "btn-primary")
                                                             ))),
                                  
                                  
                                  ## ... 2d Associated Publications ###########
                                  
                                  bsCollapse(id = "associated_pubs_bspanel",
                                             
                                             bsCollapsePanel(title="Associated Publications",           
                                                             
                                                             div(id = "associated_publications_table",
                                                                 tags$h3("Associated Publications"), 
                                                                 "Enter either a DOI or bibtex citation and click 'submit citation'.",
                                                                 
                                                                 uiOutput("title_pubs"),
                                                                 uiOutput("doi_pubs"),
                                                                 uiOutput("bibtex_pubs"),
                                                                 
                                                                 actionButton("add_pub", "Confirm and add additional publications", class = "btn-primary"),
                                                                 actionButton("add_last_pub", "Confirm and close additional publication table",
                                                                              class = "btn-primary")
                                                                 
                                                             )
                                             )), 
                                  div(id="end_study_information", 
                                      "Once you have entered all of basic information associated with your data, let's move on 
                                      to the materials and methods metadata. Select the 'submit study information' button below to finalize your 
                                      study information metadata.", tags$br(), tags$br(), 
                                      
                                      actionButton("confirm_study_tab", "Confirm all study information metadata",
                                                   class = "btn-primary")))
                    
           ),
           
           ## 3. Methods and Materials Metadata ########
           tabPanel("Methods and Materials Metadata", 
                    
                    absolutePanel(id = "universal_methods_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "3%", right = "auto", bottom = "auto",
                                  width = "94%", height = "auto", style="padding:10px;",
                                  
                                  shinyjs::useShinyjs(),
                                  
                                  div(id="intro_methods", 
                                      tags$h3("Methods and Materials Metadata"),
                                      "For each study please fill out key data regarding materials and methods that are 
                                      important to the soil carbon stocks meta-analysis. 
                                      Some users may want to include or exclude certain methodologies, 
                                      or see your commentary on the methods. Let’s make it easy for them.",
                                      tags$br(), tags$br()
                                      
                                  ),
                                  
                                  ## ... 3a Universal methods and material metadata ############

                                  bsCollapse(id = "core_methods_bspanel", 
                                             bsCollapsePanel(
                                               title="Coring Methods", 
                                               
                                               div(id = "core_methods_table", 
                                                   "Indicate the type of device used to collect soil depth profiles",
                                                   uiOutput("coring_method"), 
                                                   "Indicate whether live roots were included or excluded from carbon assessments",
                                                   uiOutput("roots_flag"),
                                                   
                                                   "Indicate the size of the sieve used on sediments prior to carbon measurements", 
                                                   tags$br(), "(millimeters)", 
                                                   uiOutput("sediment_sieve_size"),
                                                   checkboxInput("sediment_sieved_flag", "The sediment was not sieved prior to carbon measurements",
                                                                 width = "80%"),
                                                   "Indicate how the study qualified or quantified compaction of the core:",
                                                   uiOutput("compaction_flag"),
                                                   
                                                   actionButton("confirm_coring_methods", "Confirm coring methods metadata",
                                                                class = "btn-primary")
                                              
                                                   )
                                           )),
                                  
                                  ## ... 3b Carbon stock methods ###################
                                  # LOI, Bulk density 
                                  
                                  div(id = "data_types", 
                                      "Select which types of data were collected in this study:",
                                      tags$br(), tags$br()
                                  ),
                                  
                                  bsCollapse(id = "carbon_measurements_bspanel",
                                             
                                             bsCollapsePanel(title = "Carbon Stocks",
                                                             
                                                             div(id = "loi",
                                                                 checkboxInput("loi", "Loss-on-ignition"),
                                                                 
                                                                 bsCollapsePanel(           
                                                                   title = "", 
                                                                   value = "loi_var_bspanel",
                                                                   div(
                                                                     id = "loi_table",
                                                                     "Temperature at which samples were combusted to estimate fraction organic matter",
                                                                     tags$br(), "(degrees celsius)", 
                                                                     uiOutput("loi_temp"),
                                                                     "Time over which samples were combusted to estimate fraction organic matter",
                                                                     tags$br(), "(hours)", 
                                                                     uiOutput("loi_time"),
                                                                     uiOutput("loi_approx"),
                                                                     "Sample volume used for LOI, if held constant", tags$br(),
                                                                     "(cubic centimeters)", 
                                                                     uiOutput("loi_sample_volume"), 
                                                                     "Sample mass used for LOI, if held constant", tags$br(), 
                                                                     "(grams)",
                                                                     uiOutput("loi_sample_mass"),
                                                                     actionButton("confirm_loi", "Confirm and save LOI metadata", class = "btn-primary"),
                                                                     actionButton("cancel_loi", "This study did not measure LOI", class = "btn-danger")
                                                                     
                                                                   )
                                                                 ),
                                                                 checkboxInput("dbd", "Dry Bulk density"),
                                                                 bsCollapsePanel(title = "",
                                                                                 value = "dbd_bspanel",
                                                                                 div(
                                                                                   id = "dbd_table",
                                                                                   
                                                                                   "Temperature at which samples were dried to measure dry bulk density,",
                                                                                   tags$br(), "(degrees celsius)",
                                                                                   uiOutput("dbd_temp"),
                                                                                   "Time over which samples were dried to measure dry bulk density,",
                                                                                   tags$br(), "(hours)", 
                                                                                   uiOutput("dbd_time"), 
                                                                                   "Sample volume used for dry bulk density measurements, if held constant,",
                                                                                   tags$br(), "(cubic centimeters)", 
                                                                                   uiOutput("dbd_sample_volume"), 
                                                                                   "Sample mass used for dry bulk density measurements, if held constant,",
                                                                                   tags$br(), "(grams)",
                                                                                   uiOutput("dbd_sample_mass"),
                                                                                   "How did the study quantify dry bulk density?",
                                                                                   uiOutput("dbd_density"),
                                                                                   
                                                                                   actionButton("confirm_dbd", "Confirm and save dry bulk density metadata", class = "btn-primary"),
                                                                                   actionButton("cancel_dbd", "This study did not measure dry bulk density", class = "btn-danger")
                                                                                   
                                                                                 )
                                                                 ),

                                                                 checkboxInput("fraction_carbon", "Fraction carbon", width = "80%"),
                                                                 bsCollapsePanel(title = "", value = "fraction_carbon_bspanel", 
                                                                                 
                                                                                 div(id="fraction_carbon_table", 
                                                                                     "Was fraction carbon measured or estimated as a function of organic matter?",
                                                                                     uiOutput("carbon_measured_or_modeled"),
                                                                                     "Specify the method used to measure or model carbon:",
                                                                                     uiOutput("fraction_carbon_method"),
                                                                                     "Does the fraction carbon measurement refer to organic or total carbon?",
                                                                                     uiOutput("fraction_carbon_type"),
                                                                                     uiOutput("carbonates_removed"),
                                                                                     uiOutput("carbonate_removal_method"),
                                                                                     
                                                                                     actionButton("confirm_fc", "Confirm fraction carbon metadata", class = "btn-primary"),
                                                                                     actionButton("cancel_fc", "This study did not measure fraction carbon",
                                                                                                  class = "btn-danger")
                                                                                 )
                                                                 ),
                                                                 
                                                                 div(id = "end_organic_carbon_measurements", tags$br(), tags$br(),
                                                                     
                                                                    "Specify any other notes describing methodologies for determining dry bulk density, organic matter fraction, 
                                                                    and carbon fraction",
                                                                    uiOutput("carbon_profile_notes"), 
                                                                 
                                                                    actionButton("confirm_organic_carbon", 
                                                                                 "Confirm organic carbon measurement metadata",
                                                                                 class = "btn-primary"),
                                                                    actionButton("cancel_organic_carbon", 
                                                                                 "This study did not measure organic carbon",
                                                                                 class = "btn-danger")
                                                                 )
                                                          )       
                                             )
                                  ),
                                             
                                  ## ... 3c Age-depth model metadata ###########
                                  bsCollapse(id = "dating_methods_bspanel",
                                  
                                             bsCollapsePanel(title = "Age Depth Models", 
                                                             value = "ad_types_bspanel", 
                                                             
                                                             div(id="age_depth_types", 
                                                                 "Select each dating method used in your study:",
                                                                 checkboxInput("cs137", "Radiocesium (cs 137)"),
                                                                 bsCollapsePanel(title="", 
                                                                                 value = "cs137_bspanel",
                                                                                 div(id="cs137_table", 
                                                                                     "Specify the method used for determining radiocesium activity",
                                                                                     uiOutput("cs137_counting_method"),
                                                                                      actionButton("confirm_cs137", "Confirm and save cs137 metadata", class = "btn-primary"),
                                                                                      actionButton("cancel_cs137", "This study did not date using cs137", class = "btn-danger")
                                                                                 )),
                                                                 
                                                                 checkboxInput("pb210", "Lead 210 (pb 210)"),
                                                                 bsCollapsePanel(title="", 
                                                                                 value = "pb210_bspanel",
                                                                                 div(id="pb210_table", 
                                                                                     "Specify the method used for determining lead 210 actvity",
                                                                                     uiOutput("pb210_counting_method"),
                                                                                     "Specify the assumption used to estimate core's background Radium-226 activity",
                                                                                     uiOutput("ra226_assumption"),
                                                                                     
                                                                                     uiOutput("excess_pb210_exists"),
                                                                                     uiOutput("excess_pb210_model"),
                                                                                     uiOutput("excess_pb210_rate"), 
                                                                                     
                                                                                     actionButton("confirm_pb210", "Confirm and save pb210 metadata", class = "btn-primary"),
                                                                                     actionButton("cancel_pb210", "This study did not date using pb210", class = "btn-danger")
                                                                                  )
                                                                                 ),

                                                                 checkboxInput("c14", "Carbon 14"),
                                                                 bsCollapsePanel(title="", 
                                                                                 value = "c14_bspanel",
                                                                                 div(id="c14_table", 
                                                                                     "Specify the method used for determining radiocarbon activity",
                                                                                     uiOutput("c14_counting_method"),
                                                                                     actionButton("confirm_c14", "Confirm and save c14 metadata", class = "btn-primary"),
                                                                                     actionButton("cancel_c14", "This study did not date using c14", class = "btn-danger")
                                                                                     )
                                                                 ),
                                                                 
                                                                 checkboxInput("7be", "Beryillium-7 (7be)"),
                                                                 checkboxInput("241am", "Americium-241 (241am)"),
                                                                 
                                                                 uiOutput("other_marker"),
                                                                 
                                                                 uiOutput("age_depth_exists"),
                                                                 uiOutput("age_depth_model_reference"),
                                                                 uiOutput("dating_notes"), 
                                                                 
                                                                 actionButton("confirm_age_depth", "Confirm age-depth model metadata", class = "btn-primary"),
                                                                 actionButton("cancel_age_depth", "This study did not generate age-depth models", 
                                                                              class = "btn-danger")
                                                                 
                                                             ) 
                                             )
                                  ),
                                  
                                  div(id="end_methods_metadata", 
                                      "Next we will ask you about metadata associated with your sites and cores, including vegetation and human impacts. 
                                      When you're ready, select the 'Confirm materials and method metadata' button below.", tags$br(), tags$br(), 
                                      
                                      actionButton("submit_methods_metadata", "Confirm materials and method metadata",
                                                   class = "btn-primary"))
                                  
                    )
                    
           ),
           
           ## 4 Site Metadata ####################
           tabPanel("Site and Core Metadata",
                    
                    shinyjs::useShinyjs(),
                    
                    absolutePanel(id = "site_info_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "3%", right = "auto", bottom = "auto",
                                  width = "94%", height = "auto", style="padding:10px;",
                                  
                                  div(id = "site_metadata_introduction", 
                                      tags$h3("Site and Core Metadata"), 
                                      "Site information provides important context for your study. You should describe the site 
                                      and how it fits into your broader study, provide geographic information 
                                      (although this can be generated automatically from the cores as well), 
                                      and add any relevant tags and notes regarding site vegetation and inundation. 
                                      Vegetation and inundation can alternatively be incorporated into the core-level data, 
                                      whatever makes the most sense for your study design.",
                                      
                                      tags$br(), tags$br()
                                  ),
                                  
                                  bsCollapse(id = "site_metadata_bspanel",
                                             
                                             bsCollapsePanel(           
                                               title = "Site Metadata",
                                               div(
                                                 id = "site_metadata_table", 
                                                 
                                                 "Name each site associated with your data. Separate multiple sites with semicolons. 
                                                 Ensure there are no spaces within each site name.",
                                                 
                                                 uiOutput("site_ids"), 

                                                 selectInput("site_salinity_check", label = "Did you measure or observe salinity at each site?", 
                                                             choices = c("Not specified", "Yes", "No", "Only at some sites"), width="80%"),
                                                 uiOutput("site_salinity_method"),
                                                 
                                                 selectInput("site_vegetation_check", label = "Did you measure or observe the dominant vegetation type at each site?", 
                                                             choices = c("Not specified", "Yes", "No", "Only at some sites"), width="80%"),
                                                 uiOutput("site_vegetation_method"), 
                                                 
                                                 selectInput("site_inundation_check", label = "Did you measure or observe how often each site location is inundated?", 
                                                             choices = c("Not specified", "Yes", "No", "Only at some sites"), width="80%"),
                                                 uiOutput("site_inundation_method"), 
                                                 
                                                 tags$br(), tags$br(), 

                                                 actionButton("confirm_site_metadata", "Confirm site metadata", class = "btn-primary")
                                               )
                                             )
                                  ),
                                  
                                  ## 5. Core metadata ###################
                                  bsCollapse(id = "core_metadata_bspanel",
                                             
                                             bsCollapsePanel(           
                                               title = "Core Metadata",
                                               div(
                                                 id = "core_metadata_table", 
                                                 
                                                 "Name each core associated with your data. 
                                                 Separate multiple cores with semicolons. Ensure there are no spaces within each core name.",
                                                 
                                                 uiOutput("core_ids"), 
                                                 
                                                 checkboxInput("core_position_check", label="Did you determine the latitude and longitude of each core?", width = "80%"),
                                                 
                                                 bsCollapsePanel(title="", 
                                                                 value = "core_position_bspanel",
                                                                 div(id="core_position_table", 
                                                                     uiOutput("core_position_varies"),
                                                                     uiOutput("core_position"), 
                                                                     uiOutput("core_position_accuracy"),
                                                                     
                                                                     actionButton("confirm_core_position", "Confirm core position metadata")
                                                                 )
                                                 ),
                                                 
                                                 checkboxInput("core_elevation_check", label="Did you determine the elevation of each core?", width = "80%"),
                                                 
                                                 bsCollapsePanel(title="", 
                                                                 value = "core_elevation_bspanel",
                                                                 div(id="core_elevation_table", 
                                                                     
                                                                     uiOutput("core_elevation_varies"),
                                                                     uiOutput("core_elevation_datum"),
                                                                     uiOutput("core_elevation_method"), 
                                                                     uiOutput("core_elevation_accuracy"), 

                                                                     actionButton("confirm_core_elevation", "Confirm core elevation metadata")
                                                                 )
                                                 ),
                                                 
                                                 tags$br(), tags$br(), 
                                                 
                                                 selectInput("core_salinity_check", label = "Did you measure or observe salinity at each core?", 
                                                             choices = c("Not specified", "Yes", "No", "Only at some cores"), width="80%"),
                                                 uiOutput("core_salinity_method"),
                                                 
                                                 selectInput("core_vegetation_check", label = "Did you measure or observe the dominant vegetation type at each core?", 
                                                             choices = c("Not specified", "Yes", "No", "Only at some cores"), width="80%"),
                                                 uiOutput("core_vegetation_method"), 
                                                 
                                                 selectInput("core_inundation_check", label = "Did you measure or observe how often each core location is inundated?", 
                                                             choices = c("Not specified", "Yes", "No", "Only at some cores"), width="80%"),
                                                 uiOutput("core_inundation_method"), 
                                                 
                                                 uiOutput("core_impact_class"),
                                                 
                                                 tags$br(), tags$br(), 
                                                 actionButton("confirm_core_metadata", "Confirm core metadata", class = "btn-primary")
                                               )
                                             )
                                  ),
                                  
                                  actionButton("confirm_site_core_metadata", "Confirm site- and core-level metadata", class = "btn-primary")
                    )
           ),
            
           ## 6. Submit Metadata and download templates ################                                    
           tabPanel("Download Templates", 
                    absolutePanel(id = "universal_methods_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "3%", right = "auto", bottom = "auto",
                                  width = "94%", height = "auto", style="padding:10px;",
                                  
                                  shinyjs::useShinyjs(),
                                  
                                  div(id="template_panel",
                                      "Click the button to finalize your metadata and submit to the CCRCN.", tags$br(), tags$br(),
                                      actionButton("submit_metadata", "Submit metadata", class = "btn-primary"), 
                                      tags$br(), tags$br(), 
                                      "We have prepared custom templates based on the data you have collected. 
                                      Download the CSV files and fill them out and submit to the CCRCN. Raw data will likely go into the depthseries template, 
                                      while you may need to add additional metadata for each site and core. 
                                      Also attached are your responses for study information and material and methods metadata. 
                                      You can review them and make any necessary changes or simply use them as a resource in the future.", 
                                      tags$br(), tags$br(),
                                      downloadButton("template_download", "Download templates for data submission", class = "button"),
                                      
                                      tags$br(), tags$br(),
                                      
                                      tags$a(href="https://serc.si.edu/coastalcarbon/database-structure", "Visit the CCRCN database guidance"), 
                                      " to investigate the definitions of variables and CCRCN-approved values.", 
                                      
                                      tags$br(), tags$br(),
                                      
                                      actionLink("data_policy", "Read the CCRCN's data policy.")
                                      
                                      )
                    )
           ),
           
           ## 7.  Data policy ####################
           tabPanel("Data Policy",
                    
                    absolutePanel(
                      id = "memorandum", class = "panel panel-default", fixed = TRUE,
                      draggable = FALSE, top = "auto", left = "auto", right = "auto", bottom = "auto",
                      width = "70%", height = "90%", style="padding: 50px; overflow-y: scroll;",
                      
                      tags$div(
                        tags$a(href="https://serc.si.edu/coastalcarbon",tags$img(src = "ccrcn_logo.png", 
                                                                                 id= "ccrcn_logo", width="70px", height="70px"),
                               target="_blank")
                      ), 
                      
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
                        
                        "The CCRCN can assist submitters in securing space on a public data repository and issuing DOIs through 
                        our partnership with Smithsonian Libraries. We recognize that there is no official definition for what 
                        constitutes a trusted repository, but we hope that the reputation of the Smithsonian Institution, 
                        the status of FigShare as an approved technology, and the commitment of the Smithsonian Libraries to digital 
                        object curation generate this level of community trust. All data archived by the CCRCN will be 
                        listed under a ", tags$i("Creative Commons With Attribution"), " license.",
                        
                        tags$br(),tags$br(),
                        
                        "Contributors have the option of requesting an embargo period before a private dataset changes status to 
                        publically available, typically until associated manuscripts or other products are published. 
                        In this case, submitters of private data will specify a date-until-release or request so that data 
                        releases can be publicly released in conjunction with manuscripts or other products.",
                        
                        tags$br(),tags$br(),
                        
                        "Finally, CCRCN compiles publically available data into a central data clearing house. 
                        These include both data releases curated by the CCRCN and those originating from other public sources. 
                        Datasets are downloaded, reformatted to a common standard, and compiled with other studies using publically 
                        available open source R scripts. These scripts and compiled data files are available on a public GitHub 
                        repository, and accessible through the Coastal Carbon Atlas map interface. The publically available 
                        GitHub is completely separate from our data curation work flows, so there is no risk for accidental 
                        public release of private data. The CCRCN provides associated bibliographies to assist users 
                        in citing original data sources, but users are ultimately responsible for correctly citing all data used.",
                        
                        tags$br(), tags$br(), tags$br()

                      )
                    )
           )
)