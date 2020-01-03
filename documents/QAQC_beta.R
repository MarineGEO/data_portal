library(tidyverse)
library(magrittr)
library(readxl)

# Tests
# Check for empty rows & remove
# ... Check if ALL rows are empty
# Ensure site codes match
# As.numeric test

protocol_structure <- read_csv("./data_portal_prototype/data/protocol_structure.csv")

filenames <- c("fish_seines_USA-VASB_2019-09-23.xlsx",
               "seagrass_density_USA-VASB_2019-09-23.xlsx",
               "seagrass_epifauna_USA-VASB_2019-09-23.xlsx",
               "seagrass_shoots_USA-VASB_2019-09-23.xlsx")

protocols <- c("fish_seines", "seagrass_density", "seagrass_epifauna", "seagrass_shoots")

sites <- c("USA-VASB", "USA-VASB", "USA-VASB", "USA-VASB")

QA_results <- setNames(data.frame(matrix(ncol = 6, nrow = 0)), c("test", "filename", "protocol", "sheet_name", "column_name", "row_numbers"))
QA_summary <- data.frame()

all_data = list()

# Loop through each uploaded protocol
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
    protocol_df[[sheet_name]] <- read_excel(paste("./documents/secrets", filenames[i], sep="/"), 
                                            sheet = sheet_name, 
                                            na = c("NA", "This cell will autocalculate", "N/A"))
  }
  
  ## TEST numeric type 
  # Get vector of numeric and integer type columns in the given protocol
  numeric_columns <- protocol_structure %>%
    filter(protocol == current_protocol) %>%
    filter(type == "numeric" | type == "integer") %$%
    unique(.$attribute_name)
  
  for(sheet_name in protocol_sheets){

    # Extract vector of numeric columns in sheet
    sheet_numeric_columns <- subset(colnames(protocol_df[[sheet_name]]),
                                    colnames(protocol_df[[sheet_name]]) %in% numeric_columns)

    # If a sheet has numeric columns, attempt to convert them to numeric
    # If they have to coerce values to NA, the resulting warning will be logged
    if(!is.null(sheet_numeric_columns) & nrow(protocol_df[[sheet_name]]) != 0){
      
      testing_df <- protocol_df[[sheet_name]] %>%
        select(sheet_numeric_columns)
      
      numeric_test <- as.data.frame(
        apply(testing_df, c(1,2), function(x){
          tryCatch({
            value <- as.numeric(x)
            TRUE
          },
          warning = function(w){
            FALSE
          })
        }))
      
      results <- as.data.frame(which(!numeric_test, arr.ind=TRUE)) 
      
      if(nrow(results)>0){
        QA_results <- results %>%
          mutate(column_name = colnames(testing_df[col])) %>%
          group_by(column_name) %>%
          summarize(row_numbers = paste(row, collapse=", ")) %>%
          mutate(sheet_name = sheet_name,
                 protocol = current_protocol,
                 test = "Test numeric variables",
                 filename = filenames[i]) %>%
          select(test, filename, protocol, sheet_name, column_name, row_numbers) %>%
          bind_rows(QA_results)
      }
      
    }
  }

  # # Add the protocol to the overall submission data list 
  all_data[[paste(current_protocol,
                  filenames[i],
                  sites[i], sep="_")]] <- protocol_df
  
  # Set success of protocol submission
  # current_results <- QA_results %>%
  #   filter(filename == filenames[i] & site == sites[i])
  # 
  # if(all(current_results$status == "Passed")){
  #   output_metadata$status[i] <- TRUE
  #   # Currently everything is saved to curated directory
  # } else output_metadata$status[i] <- TRUE
  
}

# QA_summary <- QA_results %>%
#   group_by(filename, site) %>%
#   summarize(protocol = first(protocol), 
#             result = ifelse(any(status != "Passed"), "Submission did not pass all tests", "Submission passed"))
# 
# 
# ## Apply family test ######
# test_df <- protocol_df$sample_data
# 
# sheet_numeric_columns <- subset(colnames(test_df),
#                                 colnames(test_df) %in% numeric_columns)
# 
# which(is.na(test_df), arr.ind=TRUE)
# 
# test_df1 <- select(test_df, sheet_numeric_columns)
# 
# sapply(test_df$transect, as.numeric)
# test_df2 <- as.data.frame(
#   apply(test_df1, c(1,2), function(x){
#     tryCatch({
#       value <- as.numeric(x)
#       TRUE
#     },
#     warning = function(w){
#       FALSE
#     })
#   }))
# 
# results <- as.data.frame(which(!test_df2, arr.ind=TRUE)) %>%
#   mutate(column_name = colnames(test_df1[col])) %>%
#   group_by(column_name) %>%
#   summarize(row_numbers = paste(row, collapse=", "))
