## script containing functions to evaluate schema

## Check validity of table (Excel sheet) names #####
schemaTableNames <- function(){
  
  # Get the list of table names that should be present in the current protocol
  schema_tables <- protocol_structure %>%
    filter(protocol == current_protocol() & status == "active") 
  
  # Find absent tables (sheets missing from Excel upload) 
  absent_tables <-  schema_tables %>%
    filter(!(table %in% protocol_sheets())) %$%
    unique(.$table)
  
  # Find invalid tables (tables that aren't in the current schema)
  invalid_tables <- protocol_sheets()[!(protocol_sheets() %in% schema_tables$table)]
  
  # Construct results 
  if(length(absent_tables) >= 1){
    QA_results$df <- QA_results$df %>%
      add_row(test = "Absent table",
              sheet = absent_tables,
              protocol = current_protocol(),
              filename = original_filename_qa())
  }
  
  if(length(invalid_tables) >= 1){
    QA_results$df <- QA_results$df %>%
      add_row(test = "Invalid table",
              sheet = invalid_tables,
              protocol = current_protocol(),
              filename = original_filename_qa())
  }
  
}

## Check validity of column names #####

schemaColumnNames <- function(x){
  
  # Get the list of table names that should be present in the current protocol
  schema_tables <- protocol_structure %>%
    filter(protocol == current_protocol()) 
  
  for(current_table in protocol_sheets()){
    
    # Only check column names if the sheet/table name is present in the upload
    if(current_table %in% schema_tables$table){
      
      # Obtain any missing column names from data
      absent_columns <- schema_tables %>%
        filter(table == current_table) %>%
        filter(!(attribute %in% colnames(stored_protocol$df[[current_table]]))) %$%
        unique(.$attribute)
      
      # Determine which columns have been added by the submitter 
      column_names <- colnames(stored_protocol$df[[current_table]])
      invalid_columns <- column_names[!(column_names %in% schema_tables$attribute)]
      
      # Construct results 
      if(length(absent_columns) >= 1){
        QA_results$df <- QA_results$df %>%
          add_row(test = "Absent columns",
                  sheet = current_table,
                  column = absent_columns,
                  protocol = current_protocol(),
                  filename = original_filename_qa())
      }
      
      if(length(invalid_columns) >= 1){
        QA_results$df <- QA_results$df %>%
          add_row(test = "Invalid columns",
                  sheet = current_table,
                  column = invalid_columns,
                  protocol = current_protocol(),
                  filename = original_filename_qa())
      }
    }
  }

}

## Check validity of values ####

