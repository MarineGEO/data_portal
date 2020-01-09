library(tidyverse)
library(magrittr)
library(readxl)
library(rmarkdown)
library(knitr)

start_time <- Sys.time()

# Test data for QA/QC beta
protocol_structure <- read_csv("./data_portal_prototype/data/protocol_structure.csv")
warnings <- read_csv("./data_portal_prototype/data/warnings_lookup.csv")

filenames <- c("fish_seines_USA-VASB_2019-09-23.xlsx",
               "seagrass_density_USA-VASB_2019-09-23.xlsx",
               "seagrass_epifauna_USA-VASB_2019-09-23.xlsx",
               "seagrass_shoots_USA-VASB_2019-09-23.xlsx")

protocols <- c("fish_seines", "seagrass_density", "seagrass_epifauna", "seagrass_shoots")
sites <- c("USA-VASB", "USA-VASB", "USA-VASB", "USA-VASB")

# List to hold all uploaded spreadsheets
all_data <- list()
# List to hold generated plots
numeric_plots <- list()

# QA results stored in a list of dataframes
# Each dataframe represents an overarching type of test (numeric, testing ID relationships, etc)
QA_results <- list()
QA_results$id_relationships <- data.frame()
QA_results$numeric <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("test", "filename", "protocol", "sheet_name", "column_name", "row_numbers"))

source("./documents/QAQC_tests.R", local=TRUE)

for(i in 1:length(filenames)){
  
  current_protocol <- protocols[i]
  
  # Get names of sheets for given protocol
  protocol_sheets <- protocol_structure %>%
    filter(protocol == current_protocol) %$% # Note use of %$% rather than %>%, allows you to use $ in unique and get results as a vector
    unique(.$sheet)
  
  # Create an empty list, each object will be a sheet for the protocol
  protocol_df <- vector("list", length(protocol_sheets))
  names(protocol_df) <- protocol_sheets
  
  # Read in each sheet for the protocol, assign to respective list object 
  for(sheet_name in protocol_sheets) {
    
    df <- read_excel(paste("./documents/secrets", filenames[i], sep="/"), 
                     sheet = sheet_name, 
                     na = c("NA", "This cell will autocalculate", "N/A"))
    
    # Need to prevent empty sheets from getting uploaded
    # Needs to be recorded!
    if(nrow(df) > 0){
      protocol_df[[sheet_name]] <- df
    } else{
      protocol_sheets <- protocol_sheets[protocol_sheets != sheet_name]
    }
  }
  
  # Run QA tests
  QA_results$id_relationships <- bind_rows(QA_results$id_relationships, checkIDRelationships())
  QA_results$numeric <- bind_rows(QA_results$numeric , numericTests())
  
  # Generate visualizations
  numeric_plots[[current_protocol]] <- generateVisualizations()
  
  all_data[[paste(current_protocol, 
                  filenames[i], 
                  sites[i], sep="_")]] <- protocol_df
}

all_qa_results <- data.frame()
for(table in QA_results){
  if(nrow(table)>0){
    all_qa_results <- table %>%
      group_by(filename) %>%
      summarize(failed_tests = paste(unique(test), collapse=", ")) %>%
      bind_rows(all_qa_results)
  }
}

QA_summary <- data.frame(filenames) %>%
  rename(filename = filenames) %>%
  left_join(all_qa_results, by="filename") %>%
  mutate(failed_tests = ifelse(is.na(failed_tests), "Passed", failed_tests)) %>%
  group_by(filename) %>%
  summarize(failed_tests = paste(unique(failed_tests), collapse=", "))

rmarkdown::render(input = "./documents/test.Rmd",
                  output_format = "html_document",
                  output_file = "test.html",
                  output_dir = "./documents/")

end_time <- Sys.time()

end_time - start_time

for(protocol in numeric_plots){
  print(names(numeric_plots[1]))
  print(names(protocol))
}

