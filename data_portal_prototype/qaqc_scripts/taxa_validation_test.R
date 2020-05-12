## MarineGEO Data Submission Portal

## Taxonomic validation script for the data portal
## contact: Jaxine Wolfe, wolfejax@si.edu

library(tidyverse)

## Check submitted taxa against database ----

# read in taxa database
database <- read_csv("data_portal_prototype/data/taxa-resolved.csv")

# create list from submitted scientific_name
taxalist <- unique(df$taxon_id)

matched_taxa <- taxalist[which(taxalist %in% database$scientific_name)]

# gnr_resolve?
# create list of taxa that weren't in the database


## Resolved 
