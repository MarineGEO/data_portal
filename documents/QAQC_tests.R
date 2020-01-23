checkSampleMetadata <- function(){
  sample_metadata_results <- data.frame()
  testing_df <- protocol_df$sample_metadata 
    
  # Test that site codes are in roster
  
  # Test sample collection date
  invalid_date_index <- which(is.na(anydate(testing_df$sample_collection_date)))
  
  if(length(invalid_date_index)>0){
    # Extract invalid value at each index
    invalid_values <- pull(testing_df$sample_collection_date[invalid_date_index])
    
    sample_metadata_results <- setNames(as.data.frame(paste(invalid_date_index, collapse = ", ")), "row_numbers") %>%
      mutate(column_name = column,
             sheet_name = sheet_name,
             protocol = current_protocol,
             test = "Invalid sample collection date format",
             filename = filenames[i],
             values = paste(invalid_values, collapse=", ")) %>%
      select(test, filename, protocol, sheet_name, column_name, row_numbers, values) %>%
      bind_rows(sample_metadata_results)
  }
  
  return(sample_metadata_results)
}

checkIDRelationships <- function(){
  protocol_id_results <- data.frame()
  
  ## Check sample metadata ID links #####
  # Get unique ID variables in sample metadata sheet
  sample_metadata_ids <- protocol_structure %>%
    filter(protocol == current_protocol & sheet == "sample_metadata") %>%
    filter(id_variable == 1) %$%
    unique(.$attribute_name)
  
  # Check each data sheet to make sure unique combination of IDs are found in sample metadata page
  for(sheet_name in protocol_sheets[!protocol_sheets %in% c("sample_metadata", "taxa_list")]){
    
    # Extract IDs in data sheet that can be found in sample metadata sheet
    data_ids <- protocol_structure %>%
      filter(protocol == current_protocol & sheet == sheet_name) %>%
      filter(id_variable == 1) %>%
      filter(attribute_name %in% sample_metadata_ids) %$%
      unique(.$attribute_name)
    
    # anti_join with data sheet on left side
    # If there are rows, combine IDs for alert - represent metadata values that don't exist in the sample metadata page
    
    for(id in data_ids){
      testing_df <- protocol_df[[sheet_name]] %>%
        rowid_to_column("row")
      
      results1 <- anti_join(testing_df, protocol_df$sample_metadata, by=id) %>%
        select(id, row)
      
      if(nrow(results1)>0){
        protocol_id_results <- results1 %>%
          gather("column_name", "value", -row) %>%
          group_by(column_name, value) %>%
          summarize(row_numbers = paste(row, collapse=", ")) %>%
          mutate(sheet_name = sheet_name,
                 protocol = current_protocol,
                 test = "Invalid ID value in sample data",
                 filename = filenames[i],
                 value = as.character(value)) %>%
          select(test, filename, protocol, sheet_name, column_name, value, row_numbers) %>%
          bind_rows(protocol_id_results)
      }
      
      testing_df <- protocol_df$sample_metadata %>%
        rowid_to_column("row")
      
      results2 <- anti_join(testing_df, protocol_df[[sheet_name]], by=id) %>%
        select(id, row)
      
      if(nrow(results2)>0){
        protocol_id_results <- results2 %>%
          gather("column_name", "value", -row) %>%
          group_by(column_name, value) %>%
          summarize(row_numbers = paste(row, collapse=", ")) %>%
          mutate(sheet_name = sheet_name,
                 protocol = current_protocol,
                 test = "Invalid ID value in sample metadata",
                 filename = filenames[i],
                 value = as.character(value)) %>%
          select(test, filename, protocol, sheet_name, column_name, value, row_numbers) %>%
          bind_rows(protocol_id_results)
        
      }
    }
  }
  
  ## Check Taxa List ID relationships ####
  if("taxa_list" %in% protocol_sheets){
    
    # Get unique ID variables in sample metadata sheet
    taxa_list <- protocol_df$taxa_list
    
    # Check each data sheet to make sure unique combination of IDs are found in sample metadata page
    for(sheet_name in protocol_sheets[!protocol_sheets %in% c("sample_metadata", "taxa_list")]){
      
      if("taxon_id" %in% colnames(protocol_df[[sheet_name]])){
        
        # anti_join with data sheet on left side
        # If there are rows, combine IDs for alert - represent taxon id values that don't exist in the taxa list sheet
        
        testing_df <- protocol_df[[sheet_name]] %>%
          rowid_to_column("row")
        
        results1 <- anti_join(testing_df, taxa_list, by="taxon_id") %>%
          select(taxon_id, row)
        
        if(nrow(results1)>0){
          protocol_id_results <- results1 %>%
            # gather("column_name", "value", -taxon_id) %>%
            # group_by(column_name, value) %>%
            group_by(taxon_id) %>%
            summarize(row_numbers = paste(row, collapse=", ")) %>%
            mutate(sheet_name = sheet_name,
                   protocol = current_protocol,
                   test = "Taxon ID value in sample data not defined",
                   filename = filenames[i],
                   column_name = "taxon_id") %>%
            rename(value = taxon_id) %>%
            select(test, filename, protocol, sheet_name, column_name, value, row_numbers) %>%
            bind_rows(protocol_id_results)
        }
      }
    }
  }
  
  return(protocol_id_results)
}

## TEST numeric type 
# Get vector of numeric and integer type columns in the given protocol
numericTests <- function(){
  numeric_results <- data.frame()
  
  numeric_columns <- protocol_structure %>%
    filter(protocol == current_protocol) %>%
    filter(type == "numeric" | type == "integer") %$%
    unique(.$attribute_name)
  
  for(sheet_name in protocol_sheets){
    
    # Extract vector of numeric columns in sheet
    sheet_numeric_columns <- subset(colnames(protocol_df[[sheet_name]]),
                                    colnames(protocol_df[[sheet_name]]) %in% numeric_columns)
    
    ## Test for numeric values ####
    # If a sheet has numeric columns, attempt to convert them to numeric
    # If they have to coerce values to NA, the resulting warning will be logged
    if(!is.null(sheet_numeric_columns) & nrow(protocol_df[[sheet_name]]) != 0){
      
      testing_df <- protocol_df[[sheet_name]] %>%
        select(sheet_numeric_columns)
      
      numeric_test <- as.data.frame(
        apply(testing_df, c(1,2), function(x){
          tryCatch({
            value <- as.numeric(x)
            TRUE
          },
          warning = function(w){
            FALSE
          })
        }))
      
      results <- as.data.frame(which(!numeric_test, arr.ind=TRUE)) 
      
      if(nrow(results)>0){
        numeric_results <- results %>%
          mutate(column_name = colnames(testing_df[col])) %>%
          group_by(column_name) %>%
          summarize(row_numbers = paste(row, collapse=", ")) %>%
          mutate(sheet_name = sheet_name,
                 protocol = current_protocol,
                 test = "Invalid characters in numeric attribute",
                 filename = filenames[i]) %>%
          select(test, filename, protocol, sheet_name, column_name, row_numbers) %>%
          bind_rows(numeric_results)
      }
      
      numeric_results <- bind_rows(numeric_results,
                                   numericMinMaxTest(numeric_results, current_protocol, sheet_name, testing_df))
    }
  }
  
  return(numeric_results)
  
}

## Test for valid minimum and maximum values ####
numericMinMaxTest <- function(numeric_results, current_protocol, sheet_name, testing_df){
  
  protocol_minmax <- protocol_structure %>%
    filter(protocol == current_protocol & sheet == sheet_name & attribute_name %in% colnames(testing_df))
  
  numeric_results <- data.frame()
  
  for(column in colnames(testing_df)){
    
    ## ... Minimum Test ####
    # Find the index for any values that are less than the attribute's minimum
    min_invalid_row_index <- which((testing_df[column] < filter(protocol_minmax, attribute_name == column)$minimum_warning) 
                                   %in% 1) 
    # If there are invalid values:
    if(length(min_invalid_row_index) > 0){
      # Extract invalid value at each index
      invalid_values <- pull(testing_df[column])[min_invalid_row_index]
      
      numeric_results <- setNames(as.data.frame(paste(min_invalid_row_index, collapse = ", ")), "row_numbers") %>%
        mutate(column_name = column,
               sheet_name = sheet_name,
               protocol = current_protocol,
               test = "Invalid minimum value",
               filename = filenames[i],
               values = paste(invalid_values, collapse=", ")) %>%
        select(test, filename, protocol, sheet_name, column_name, row_numbers, values) %>%
        bind_rows(numeric_results)
    }
    
    ## ... Maximum Test ####
    # Find the index for any values that are greater than the attribute's maximum
    max_invalid_row_index <- which((testing_df[column] > filter(protocol_minmax, attribute_name == column)$maximum_warning)
                                   %in% 1) 
    # If there are invalid values:
    if(length(max_invalid_row_index) > 0){
      # Extract invalid value at each index
      invalid_values <- pull(testing_df[column])[max_invalid_row_index]
      
      numeric_results <- setNames(as.data.frame(paste(max_invalid_row_index, collapse = ", ")), "row_numbers") %>%
        mutate(column_name = column,
               sheet_name = sheet_name,
               protocol = current_protocol,
               test = "Invalid maximum value",
               filename = filenames[i],
               values = paste(invalid_values, collapse=", ")) %>%
        select(test, filename, protocol, sheet_name, column_name, row_numbers, values) %>%
        bind_rows(numeric_results)
    }
    
  }
  return(numeric_results)
}

# Generate visualizations for RMarkdown document
generateVisualizations <- function(){
  # Histograms of each non-ID numeric column
  visualizations <- list()
  
  numeric_columns <- protocol_structure %>%
    filter(protocol == current_protocol) %>%
    filter((type == "numeric" | type == "integer") & is.na(id_variable)) %$%
    unique(.$attribute_name)
  
  for(sheet_name in protocol_sheets[!protocol_sheets %in% c("sample_metadata", "taxa_list")]){
    
    # Extract vector of numeric columns in sheet
    sheet_numeric_columns <- subset(colnames(protocol_df[[sheet_name]]),
                                    colnames(protocol_df[[sheet_name]]) %in% numeric_columns)
    
    # If a sheet has numeric columns, attempt to convert them to numeric
    # If they have to coerce values to NA, the resulting warning will be logged
    if(!is.null(sheet_numeric_columns) & nrow(protocol_df[[sheet_name]]) != 0){
      
      visualizations[[sheet_name]] <- protocol_df[[sheet_name]] %>%
        select(sheet_numeric_columns) %>%
        gather() %>% 
        ggplot(aes(value)) +
        facet_wrap(~ key, scales = "free") +
        geom_histogram()
    }
  }
  
  return(visualizations)

}

