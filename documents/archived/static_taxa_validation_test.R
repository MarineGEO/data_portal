## MarineGEO Data Submission Portal

## ARCHIVED SCRIPT

## Taxonomic validation script for the data portal
## contact: Jaxine Wolfe, wolfejax@si.edu

library(tidyverse)
library(worrms)
library(taxize)
library(plyr)

# Function Overview:
# 1) Input list of taxa
# 2) Check submitted taxa against database
# 3) Resolve taxa names that aren't documented in the database
# 4) Query WORMS to validate undocumented, resolved taxa
# 5) Notify user of undocumented taxa that are validated candidates for the database
# 6) Notify user of undocumented, unresolved taxa

testTaxa <- function(df){
  
  # source validateTaxa()
  source("documents/taxonomy_functions.R")
  
  # create list from submitted scientific_name
  taxalist <- unique(df$scientific_name)
  
  ## Check submitted taxa against database ----
  
  # read in taxa database
  database <- read_csv("data_portal_prototype/data/taxa-database-valid.csv")
  # isolate all user-submitted and resolved taxa
  database_taxa <- unique(c(database$scientific_name, database$resolved_name))
  
  # check for taxa in database, store undocumented taxa
  undocumented <- taxalist[which(!(taxalist %in% database_taxa))]
  
  # Resolve taxa names that aren't documented in the database
  if (length(undocumented) > 0){
    
    resolved <- data.frame()
    unresolved <- vector()
    
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
        resolved <- rbind(resolved, gnr_result)
        
      } else {
        # compile list of unresolved taxa
        unresolved <- c(unresolved, undocumented[i])
        # skip unresolved taxa
        i <- i + 1
        next
      }
    }
  } else {
    print("Looks good! All taxa are verified in the MarineGEO database.")

    # prevent an error message from displaying
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }
  
  if (length(resolved) > 0) {
    # # validate the resolved names
    # validated <- validateTaxa(resolved$matched_name2)
    # 
    # # merge resolved and validated data frames
    # resolved_validated <- resolved %>%
    #   rename(scientific_name = user_supplied_name,
    #          resolved_name = matched_name2,
    #          data_source = data_source_title,
    #          gnr_score = score) %>%
    #   left_join(., validated, by = "resolved_name") %>%
    #   select(-submitted_name)
      
    # write updated database to documents folder?
    # check that the taxa resolved correctly
    print("Please review the following taxa before adding to the database:")
    print(resolved$matched_name2)
    # return(resolved_validated)
  }
  
  if (length(unresolved) > 0) {
    print("The following taxa could not be resolved:")
    print(unresolved)
  }
  
  return(resolved_validated)
}


# read in test data
# df <- read_csv("documents/test_data/test_taxa.csv")

# run function
# the output could probably be bundled better
# test <- testTaxa(df = df)


 