## MarineGEO Data Submission Portal

## Taxonomic validation script for the data portal
## contact: Jaxine Wolfe, wolfejax@si.edu

library(tidyverse)
library(worrms)
library(taxize)
library(plyr)

# read in test data
df <- read_csv("documents/test_data/test_taxa.csv")


# Cases to address:
# taxa not in the database => resolve and add to database
# taxa cannot be resolved => let the data provider know in the report
# taxa scientific name is outdated (script doesn't currently use the worrms package)

resolveTaxa <- function(df){
  # for(sheet_name in protocol_sheets()){  
  # will need to harvest taxa from each sheet (protocol_structure?)
  # notify data provider of unresolved taxa (and which sheet they're on?)
  # }
  
  ## Check submitted taxa against database ----
  
  # read in taxa database
  database <- read_csv("data_portal_prototype/data/taxa-resolved.csv")
  
  # create list from submitted scientific_name
  taxalist <- unique(df$scientific_name)
  
  # check for taxa in database, store undocumented taxa
  # documented <- taxalist[which(taxalist %in% database$scientific_name)]
  undocumented <- taxalist[which(!(taxalist %in% database$scientific_name))]
  
  # Resolve taxa names that aren't documented in the database
  if (length(undocumented) > 0){
    
    resolved <- data.frame()
    unresolved <- vector()
    
    for (i in 1:length(undocumented)){
      gnr_result <- gnr_resolve(names = as.vector(undocumented[i]), 
                                canonical = TRUE,
                                # gnr_datasources()
                                preferred_data_sources = c(9, 4, 3)) %>%
        # pick the first result
        slice(1)
      
      if (!empty(gnr_result)) {
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
  }
  
  if (length(resolved) > 0) {
    # check that the resolved name isn't documented
    # if(any(gnr_result$matched_name2 %in% database$resolved_name)){}
    
    # add undocumented resolved 
    update_db <- resolved %>%
      rename(scientific_name = user_supplied_name,
             resolved_name = matched_name2,
             data_source = data_source_title,
             match_score = score) %>%
      select(-submitted_name) %>%
      bind_rows(database, .) %>%
      arrange(resolved_name)
    
    # write updated database to documents folder?
    # check that the taxa resolved correctly
    print("The following resolved taxa were added to the database:")
    print(resolved$matched_name2)
  }
  
  if (length(unresolved) > 0) {
    print("The following taxa could not be resolved:")
    print(unresolved)
  }
}

# resolveTaxa(df = df)


 