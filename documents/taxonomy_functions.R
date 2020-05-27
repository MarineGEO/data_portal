## MarineGEO Data Submission Portal

## Taxonomic functions for the data portal
## contact: Jaxine Wolfe, wolfejax@si.edu

# library(tidyverse)
# library(worrms)
# library(taxize)

resolveTaxa <- function(taxa) {
  
  resolved <- data.frame()
  unresolved <- vector()
  
  for (i in 1:length(taxa)){
    # store resolved results
    gnr_result <- gnr_resolve(names = as.vector(taxa[i]), 
                              canonical = TRUE,
                              # gnr_datasources()
                              preferred_data_sources = c(9, 4, 3)) %>%
      # pick the first result
      slice(1)
    
    if (!plyr::empty(gnr_result)) {
      # compile list of resolved taxa
      resolved <- rbind(resolved, gnr_result)

    } else {
      # save unresolved taxa
      unresolved <- rbind(unresolved, taxa[i])
      # skip unresolved taxa
      i <- i + 1
      next
    }
  }
  
  if (length(unresolved) > 0) {
    print("The following taxa could not be resolved:")
    print(unresolved)
  }
  
  return(resolved)
}

# taxa <- read_csv("documents/test_data/test_update_taxa.csv")$Species
# test_taxa <- resolveTaxa(taxa)

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
        # inclusion of this statement makes the script more conservative & less presumptive
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
    mutate(name_updated = ifelse(resolved_name == valid_name, 
                               yes = FALSE,
                               no = TRUE))
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

# Classify database taxa ----
classifyTaxaDatabase <- function() {
  # read in taxa database
  taxa_database <- read_csv("data_portal_prototype/data/taxa-database-valid.csv") %>%
    drop_na(valid_AphiaID)
  
  # isolate aphia ID
  ids <- unique(taxa_database$valid_AphiaID)
  
  classify_taxa <- data.frame()
  
  for (i in 1:length(ids)){
    # classify taxa
    classify <- wm_classification(ids[i])
    
    # get taxa name 
    # temp_taxa <- wm_id2name(ids[1])
    temp_taxa <- classify$scientificname[nrow(classify)]
    
    # add original taxa information to classification
    # for table reformatting and relation to the database
    temp_classify <- classify %>% mutate(valid_name = temp_taxa,
                                         valid_AphiaID = ids[i]) %>%
      select(-AphiaID)
    
    # compile taxa classifications
    classify_taxa <- bind_rows(classify_taxa, temp_classify)
    
  }

  # convert classification table to wide format
  classify_taxa_wide <- classify_taxa %>% 
    # double superclass Gnathostomata and Pisces prevents wide format
    filter(rank != "Superclass") %>%
    pivot_wider(id_cols = c(valid_name, valid_AphiaID),
                names_from = rank,
                values_from = scientificname)
  
  return(classify_taxa_wide)
}

# update taxa database with candidate taxa from taxa-resolved.csv
updateTaxaDatabase <- function(){
  
  # if taxa-resolved exists, validate and update 
  if (file.exists("taxa-resolved.csv")){
    
    taxa <- read_csv("data_portal_prototype/data/taxa-resolved.csv")
    
    # validate the resolved names
    validated <- validateTaxa(taxa$matched_name2)
    
    # merge resolved and validated data frames
    resolved_validated <- taxa %>%
      rename(scientific_name = user_supplied_name,
             resolved_name = matched_name2,
             data_source = data_source_title,
             gnr_score = score) %>%
      left_join(., validated, by = "resolved_name") %>%
      select(-submitted_name)
    
  } else {
    print("Already up to date.")
  }
}

# Run once and a while to make sure the database is up to date on valid taxa
# validateTaxaDatabase <- function(candidate_taxa) {
#   
# }