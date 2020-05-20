## MarineGEO Data Submission Portal

## ARCHIVED SCRIPT
## Please use updated validation function: validateTaxa()

## Script to validate taxa in marinegeo database
## contact: Jaxine Wolfe, wolfejax@si.edu

library(tidyverse)
library(taxize)
library(worrms)

database <- read_csv("data_portal_prototype/data/taxa-database.csv")

# Query WORMS for valid name ----

# Purpose: make sure that names aren't outdated
# ex: Cymodocea filiiformis
taxa <- unique(database$resolved_name)

unresolved_taxa <- c("Brachiosyllis", "Macrobrachium intermedium", 
                     "Nassarius tegula", "Pleuronectoidei", "Ventricularia")
freshwater_taxa <- c("Odostomia bisuturalis", "Gammarus lecroyae",
                     "Palaemonetes pugio", "Periclimenes pedersoni", "Radix balthica",
                     "Theodoxus fluviatilis")
problem_taxa <- unique(filter(database, data_source == "ITIS" | data_source == "NCBI")$resolved_name)
problem_taxa <- c(problem_taxa[!(problem_taxa %in% unresolved_taxa)], freshwater_taxa)

validated_taxa <- vector()
valid_taxa_id <- vector()

for (j in 1:length(taxa)){
  # other (unuseful) functions
  # taxa_record <- wm_record_(name = taxa[j]) # deprecated
  # taxa_id <- wm_name2id(taxa[j])
  
  ## Validate taxa (if possible) ----
  if(taxa[j] %in% unresolved_taxa){
    validated_taxa[j] <- NA
    valid_taxa_id[j] <- NA
    
    j <- j + 1
    next
    
  } else if (taxa[j] %in% problem_taxa) {
    
    fuzzy_taxa <- worrms::wm_records_taxamatch(taxa[j],
                                               # some species are't labeled as marine
                                               marine_only = FALSE)
    taxa_record <- as.data.frame(fuzzy_taxa)
    
  } else {
    worms_id <- taxize::get_wormsid(taxa[j], 
                                    accepted = FALSE,
                                    # not sure if rows should be specified
                                    rows = 1)
    
    taxa_id <- as.data.frame(worms_id)$ids
    taxa_record <- worrms::wm_record(as.numeric(taxa_id))
    
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

join_validation <- data.frame(resolved_name = taxa, 
                   valid_name = validated_taxa,
                   valid_AphiaID = valid_taxa_id,
                   stringsAsFactors = FALSE) %>%
  mutate(name_match = ifelse(resolved_name == valid_name, 
                              yes = TRUE,
                              no = FALSE))

invalid_entries <- join_validation %>% filter(name_match == FALSE)
unresolved <- join_validation %>% filter(is.na(valid_name))

valid_database <- left_join(database, join_validation, by = "resolved_name")

write_csv(valid_database, "data_portal_prototype/data/taxa-database-valid.csv")
