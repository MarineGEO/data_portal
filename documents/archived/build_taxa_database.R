## MarineGEO Data Submission Portal

## ARCHIVED SCRIPT

## Script to generate a database of taxa from previous data submissions
## contact: Jaxine Wolfe, wolfejax@si.edu

library(tidyverse)
library(plyr)
library(data.table)
library(taxize)
# library(worrms)

## Creating a MarineGEO taxa database ----

# list all files
files <- list.files(path = "/Users/jaxinewolfe/Dropbox (Smithsonian)/MarineGEO/Data/curated_directory", 
                         pattern = "*.csv",
                         full.names = TRUE,
                         recursive = TRUE)

# isolate files with taxon_id info
taxon_id_files <- files[which(!(grepl("metadata|taxa-list|environmental", files)))]

# isolate files with taxon lists
taxa_list_files <- files[which(grepl("taxa-list", files))]

# Read in taxon_id_files and isolate taxon_id
# initiate a blank data frame
taxa_log <- data.frame()

for (i in 1:length(taxon_id_files)){
  
  # read in files using the fread function from the data.table package
  temp_data <- fread(taxon_id_files[i])
  
  if("taxon_id" %in% names(temp_data)){
    temp_data <- temp_data %>%
      select(taxon_id) %>%
      distinct()
    
  }else{
    i <- i + 1
    next
  }

  count <- i + 1
  
  # for each iteration, bind the new data to the compiled dataset
  taxa_log <- rbind(taxa_log, temp_data) %>%
    drop_na() %>%
    distinct()
}

# Read in taxa lists and isolate taxon_id and scientific_name
# initiate a blank data frame
list_log <- data.frame()

for (i in 1:length(taxa_list_files)){
  
  # read in files using the fread function from the data.table package
  temp_list <- fread(taxa_list_files[i]) %>%
    select(taxon_id, scientific_name) %>%
    distinct()
  
  count <- i + 1
  
  # for each iteration, bind the new data to the compiled dataset
  list_log <- rbind(list_log, temp_list) %>%
    distinct()
}

# not sure how effective this merge is
# there are identical taxon_ids for different scientific names
taxon_all <- full_join(taxa_log, list_log, by = "taxon_id")

# scientific names with no corresponding taxon_id
# na_taxon_id <- sort(taxon_all$taxon_id[which(is.na(taxon_all$scientific_name))])
# there are no species in the taxon_id that aren't covered in the scientific name
# taxon_id will be mostly irrelevant in the updated fieldsheets

## Resolve Taxa Names ----

taxa_resolved <- data.frame(original_name = sort(unique(as.character(taxon_all$scientific_name))), 
                            resolved_name = NA,
                            data_source = NA,
                            match_score = NA)

for (k in 1:nrow(taxa_resolved)) {
  
  # try to resolve the name with gnr
  gnr_result <- gnr_resolve(names = as.vector(taxa_resolved$original_name[k]), 
                            canonical = TRUE,
                            # best_match_only = TRUE,
                            preferred_data_sources = c(9, 4, 3))
  # 9 = worms
  # 4 = ncbi
  # 3 = itis
  # hardcoded from data sources: https://resolver.globalnames.org/data_sources
  
  # gnr_resolve(names = "Syringodium filiforme",
  #             canonical = TRUE,
  #             # best_match_only = TRUE,
  #             preferred_data_sources = 9)
  
  if (!empty(gnr_result)) {
    # store resolved name and data source
    gnr_name <- unlist(gnr_result[1, 'matched_name2'])
    authority <- unlist(gnr_result[1, 'data_source_title'])
    gnr_score <- unlist(gnr_result[1, 'score'])
    
    # populate resolved name and data source columns
    taxa_resolved$resolved_name[k] <- gnr_name
    taxa_resolved$data_source[k] <- authority
    taxa_resolved$match_score[k] <- gnr_score
    
  } else {
    # skip unresolved taxa
    k <- k + 1
    next
  }
}

taxa_resolved <- taxa_resolved %>%
  # idk why this reverted to factor
  mutate(original_name = as.character(original_name))

# isolate unresolved taxa
unresolved <- as.character(taxa_resolved$original_name[which(is.na(taxa_resolved$resolved_name))])

# check non-primary data souces (NCBI and ITIS)
# taxa_resolved %>% filter(data_source == "NCBI" | data_source == "ITIS")
# check low scores
# taxa_resolved %>% filter(match_score < 0.75)

# Create Database table ----

filter_out <- c("Red", "Epiphyes")

database <- left_join(taxon_all, taxa_resolved, 
                                 by = c("scientific_name" = "original_name")) %>%
  filter(!(resolved_name %in% filter_out)) %>%
  filter(!is.na(resolved_name))


# write all the data
write.csv(taxon_all, "data/taxa-original.csv", row.names = FALSE)
write.csv(database, "data/taxa-resolved.csv", row.names = FALSE)
write.csv(as.data.frame(unresolved), "data/taxa-unresolved.csv", row.names = FALSE)


