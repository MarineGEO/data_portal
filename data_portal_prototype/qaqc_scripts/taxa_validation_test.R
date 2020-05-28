## MarineGEO Data Submission Portal

## Taxonomic validation script for the data portal
## contact: Jaxine Wolfe, wolfejax@si.edu

library(tidyverse)
# library(worrms)
library(taxize)
library(plyr)

# read in test data
df <- read_csv("documents/test_data/test_taxa.csv")

# Cases to address:
# taxa not in the database => resolve and add to database
# taxa cannot be resolved => let the data provider know in the report
# taxa scientific name is outdated (script doesn't currently use the worrms package)

# Workflow:
# 1) Input protocol
# 2a) Extract list of taxa from each protocol sheet
# 2b) Check submitted taxa against database
# 3) Resolve taxa names that aren't documented in the database
# 4) Query WORMS to validate undocumented, resolved taxa
# 5) Notify user of undocumented taxa that are validated candidates for the database
# 6) Notify user of undocumented, unresolved taxa

testTaxa <- function(df){
  
  source("documents/taxonomy_functions.R")
  
  results <- data.frame()
  resolved <- data.frame()
  unresolved <- vector()
  
  tryCatch({
    ## Acquire taxa list from each protocol ----
    
    # Pull out any scientific names in protocol sheets
    # taxa_column <- protocol_structure %>%
    #   filter(protocol == current_protocol()) %$%
    #   unique(.$scientific_name)
    
    for(sheet_name in protocol_sheets()){
      
      if("scientific_name" %in% colnames(stored_protocol$df[[sheet_name]])){
        
        # create list from submitted scientific_name
        # taxalist <- unique(df$scientific_name) # static version
        taxalist <- stored_protocol$df[[sheet_name]] %$%
          unique(.$scientific_name)

      ## Check submitted taxa against database and previous protocol sheets ----
      
      # read in taxa database
      database <- read_csv("data_portal_prototype/data/taxa-database-valid.csv")
      # isolate all user-submitted and resolved taxa
      database_taxa <- unique(c(database$original_name, database$resolved_name))
      
      # if(exists("undocumented")){
      #   # remove taxa from undocumented that have already been processed on previous sheets
      #   taxalist <- taxalist[which(!(taxalist %in% undocumented))]
      # }
      
      # check for taxa in database, store undocumented taxa
      undocumented <- taxalist[which(!(taxalist %in% database_taxa))]
      
      # Resolve taxa names that aren't documented in the database
      if (length(undocumented) > 0){
        
        for (i in 1:length(undocumented)){
          # store resolved results
          gnr_result <- gnr_resolve(names = as.vector(undocumented[i]), 
                                    canonical = TRUE,
                                    # gnr_datasources()
                                    preferred_data_sources = c(9, 4, 3)) %>%
            # pick the first result
            slice(1)
          
          if (!plyr::empty(gnr_result)) {
            # compile list of resolved taxa
            resolved <- bind_rows(resolved, gnr_result)
            
          } else {
            # compile list of unresolved taxa
            unresolved <- c(unresolved, undocumented[i])
            # skip unresolved taxa
            i <- i + 1
            next
          }
        }
        # store undocumented of the current sheet to be 
        # previous_undocumented <- undocumented
        
      } else {
        
        # Case: All taxa from the current sheet are verified in the MarineGEO database.
        # Go to the next sheet
        next
      }
      
      # this output will be stored in a reactive value and saved to the data folder
      # if (length(resolved) > 0) {
        
        # # Save to data folder
        # if(file.exists("taxa-resolved.csv")){
        #   # update existing taxa-resolved file
        #   existing_resolved <- read_csv("data_portal_prototype/data/taxa-resolved.csv")
        #   updated_resolved <- row_bind(existing_resolved, resolve)
        #   # write updated taxa for review
        #   write_csv(updated_resolved, "data_portal_prototype/data/taxa-resolved.csv")
        # } else {
        #   # write taxa for review
        #   write_csv(resolved, "data_portal_prototype/data/taxa-resolved.csv")
        # }
        
        # write updated database to documents folder?
        # check that the taxa resolved correctly
        # print("Please review the following taxa before adding to the database:")
        # print(resolved$matched_name2)
      # }
      
      # this output will be stored in a reactive value and saved to the data folder
      if (length(unresolved) > 0) {
        
        # Save UNRESOLVED to data folder ----
        # if(file.exists("taxa-unresolved.csv")){
        #   # update existing taxa-unresolved file
        #   existing_unresolved <- read_csv("data_portal_prototype/data/taxa-unresolved.csv")
        #   updated_unresolved <- row_bind(existing_unresolved, unresolved)
        #   # write updated taxa for review
        #   write_csv(updated_unresolved, "data_portal_prototype/data/taxa-unresolved.csv")
        # } else {
        #   # write taxa for review
        #   write_csv(unresolved, "data_portal_prototype/data/taxa-unresolved.csv")
        # }
        
        # QAQC results
        print("The following taxa could not be resolved:")
        print(unresolved)
        
        # record inability to resolve taxa
        # a table with the columns: protocol, protocol_sheet, unresolved_name
        results <- setNames(as.data.frame("Taxonomic validation"), "test") %>%
          mutate(filename = original_filename_qa(),
                 protocol = current_protocol(),
                 sheet_name = sheet_name,
                 column_name = "scientific_name",
                 values = unresolved,
                 row_numbers = NA) %>%
          select(test, filename, protocol, sheet_name, column_name, values, row_numbers) %>%
          bind_rows(results)
      }
      }
    }
    return(results)
  },
  error = function(e){
    print(e)

    # Create and return an error message in the QA result log 
    setNames(as.data.frame("Error testing taxa"), "test") %>%
      mutate(column_name = NA,
             sheet_name = NA,
             protocol = current_protocol(),
             filename = original_filename_qa(),
             values = NA,
             row_numbers = NA) %>%
      select(test, filename, protocol, sheet_name, column_name, row_numbers, values) 
  })
}
