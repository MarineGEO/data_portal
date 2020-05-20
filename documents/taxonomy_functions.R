## MarineGEO Data Submission Portal

## Taxonomic functions for the data portal
## contact: Jaxine Wolfe, wolfejax@si.edu

# library(tidyverse)
# library(worrms)
# library(taxize)

# resolveTaxa <- function(taxa) {
# 
# }

# updateTaxaDatabase <- function(candidate_taxa) {
#   
# }

# Query WORMS to validate undocumented, resolved taxa ####
validateTaxa <- function(taxa) {
  
  validated_taxa <- vector()
  valid_taxa_id <- vector()
  
  for (j in 1:length(taxa)){
    # other (unuseful) functions
    # taxa_record <- wm_record_(name = taxa[j]) # deprecated
    # taxa_id <- wm_name2id(taxa[j])
    
    ## Validate taxa (with error handeling) ----
    worms_id <- tryCatch(
      # not sure if rows should be specified
      taxize::get_wormsid(taxa[j], accepted = FALSE, rows = 1),
      error = function(e) e
    )
    
    if(!inherits(worms_id, "error")){
      # assign valid name and aphia id
      taxa_id <- as.data.frame(worms_id)$ids
      taxa_record <- worrms::wm_record(as.numeric(taxa_id))
      
    } else {
      fuzzy_taxa <- tryCatch(
        # some species are't labeled as marine
        worrms::wm_records_taxamatch(taxa[j], marine_only = FALSE), 
        error = function(e) e)
          # error message
          # message(paste("Taxa does not seem to exist in the WORMS database:", taxa[j]))
          # message("Original error message:")
          # message(e)
      
      # Case: the fuzzy match produced no taxa records
      if(inherits(fuzzy_taxa, "error")) {
        validated_taxa[j] <- NA
        valid_taxa_id[j] <- NA
        
        j <- j + 1
        next
        
        # Case: the taxa match record does not contain an exact name match
        # will wm_records_taxamatch() ever return >1 records with an exact name match? 
      } else if (!any(taxa[j] %in% as.data.frame(fuzzy_taxa)$scientificname)) {
        validated_taxa[j] <- NA
        valid_taxa_id[j] <- NA
        
        j <- j + 1
        next
        
        # proceed
      } else {
        taxa_record <- as.data.frame(fuzzy_taxa)
      }
    }
    
    ## Assign valid names and ids ----
    if (taxa_record$status == "alternate representation") {
      current_name <- taxa_record$scientificname
      current_id <- taxa_record$AphiaID
      
    } else {
      current_name <- taxa_record$valid_name
      current_id <- taxa_record$valid_AphiaID
    }
    
    validated_taxa[j] <- current_name
    valid_taxa_id[j] <- current_id    
  }
  
  # create dataframe for validated taxa
  validation <- data.frame(resolved_name = taxa, 
                           valid_name = validated_taxa,
                           valid_AphiaID = valid_taxa_id,
                           stringsAsFactors = FALSE) %>%
    mutate(name_match = ifelse(resolved_name == valid_name, 
                               yes = TRUE,
                               no = FALSE))
  # return validation df
  return(validation)
}

## test validateTaxa ----
# taxa_database <- read_csv("data_portal_prototype/data/taxa-database-valid.csv") 
# Subset database to create a trial taxalist 
# taxa_unresolved <- filter(taxa_database, is.na(valid_name))$resolved_name
# taxa_fresh <- c("Odostomia bisuturalis", "Gammarus lecroyae",
#                      "Palaemonetes pugio", "Periclimenes pedersoni", "Radix balthica",
#                      "Theodoxus fluviatilis")
# taxa_invalid <- filter(taxa_database, name_match == FALSE)$resolved_name[runif(5, min = 1, max = 37)]
# taxa_valid <- filter(taxa_database, 
#                      name_match == TRUE)$resolved_name[runif(5, min = 1, max = 400)]
# # trial taxa
# taxa <- c(taxa_fresh, taxa_unresolved, taxa_invalid, taxa_valid)
# # run function
# test <- validateTaxa(taxa)
