
## Update taxa database

source("documents/taxonomy_functions.R")

database <- read_csv("data_portal_prototype/data/taxa-database-valid.csv")

# unresolved has to be bind_row to retain spelling variation
unresolved <- read_csv("data_portal_prototype/data/taxa-unresolved.csv") %>%
  filter(!is.na(manually_resolved_name))

# resolve unresolved
resolve_unresolved <- resolveTaxa(unique(unresolved$manually_resolved_name))
# problem: sometimes taxa that don't resolve work in validateTaxa() (i.e. "Polyides rotunda")
# the manually resolved taxa should be pretty well-vetted
# plug right into validateTaxa() ?

validate_resolved <- validateTaxa(unique(unresolved$manually_resolved_name))

# merge 
merge_resolved_valid <- full_join(resolve_unresolved, validate_resolved,
                           by = c("matched_name2" = "resolved_name")) %>%
  rename(resolved_name = matched_name2,
         gnr_score = score) %>%
  mutate(name_updated = ifelse(resolved_name == valid_name,
                               yes = FALSE,
                               no = TRUE)) %>%
  filter(!is.na(valid_name)) %>%
  select(-c(submitted_name, user_supplied_name))

goldstar_taxa <- left_join(merge_resolved_valid, unresolved,
                           by = c("resolved_name" = "manually_resolved_name"))
  

# add to database
database_new <- bind_rows(database, goldstar_taxa) 
  # arrange(resolved_name)

# write_csv(database_new, "data_portal_prototype/data/taxa-database-valid.csv")