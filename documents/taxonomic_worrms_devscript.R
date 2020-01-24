library(worrms)

df <- all_data$`fish_seines_fish_seines_USA-VASB_2019-09-23.xlsx_USA-VASB`$taxa_list

snames <- c(df$scientific_name,"garbly gook")

results <- wm_records_names(name = snames)

results_df <- do.call(rbind, lapply(results, as.data.frame))

missing_ids <- snames[!(snames %in% results_df$scientificname)]
