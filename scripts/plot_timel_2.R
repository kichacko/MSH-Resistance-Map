function_plot_timel_2 = function(input_data, subset_isolate){
  
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data
  
  
  
  ################# SUBSET ######################
  
  
  
  # Subset
  df_02 = subset(df_01, df_01$Isolate.ID %in% subset_isolate)
  df_02 = subset(df_02, select = c("Procedure", "Pathogen", "Agent", "MIC"))
  df_03 = merge(df_01, df_02, by = c("Procedure", "Pathogen", "Agent", "MIC"))
  df_03 = subset(df_03, select = c("Patient.ID", "Isolate.ID", "Pathogen", "Agent", "MIC", "Collection.Date", "Unit"))
  df_03 = unique(df_03)
  
  
  
  ################# ID ######################
  
  
  
  # Create custom ID
  df_03 = transform(
    df_03,
    ID = match(
      df_03$`Patient.ID`,
      unique(df_03$`Patient.ID`)
      
    )
  )
  
  
  
  ################# MATRIX ######################
  
  
  
  
  # matrix_01
  matrix_01 = dcast(
    data          = df_03,
    formula       = df_03$Patient.ID + df_03$Isolate.ID + df_03$Unit ~ df_03$Agent + df_03$MIC,
    value.var     = "MIC",
    fun.aggregate = length,
    fill          = NA_real_
    
  )
  
  colnames(matrix_01)[1] = "Patient"
  colnames(matrix_01)[2] = "Isolate"
  colnames(matrix_01)[3] = "Unit"
  
  
  
  ################# CALCULATIONS ######################
  
  
  
  # Create a "Unique data" frame that identifies the unique resistance profiles, sorts them by a pseudo intensitiy, and creates a unique ID for each
  unique_data         = matrix_01
  unique_data[1:3]    = NULL
  unique_data         = unique(unique_data)
  unique_data$ColSum  = rowSums(unique_data)
  unique_data         = unique_data[order(unique_data[["ColSum"]]),]
  unique_data$ID      = 1:nrow(unique_data)
  
  # Match these IDs back to the original matrix_01
  combined_data = merge(unique_data, matrix_01)
  # combined_data = combined_data[row.names(unique(combined_data[,c("ID", "Patient")])),]  # So you can remove isolates from patients with the same ID
  combined_data = subset(combined_data, select = c("ID", "Patient", "Isolate", "Unit"))
  
  
  matrix_02 = dcast(
    data = combined_data,
    formula = combined_data$Patient + combined_data$Isolate + combined_data$Unit ~ combined_data$ID,
    value.var = "ID",
    fun.aggregate = length
    
  )
  
  
  # Filter for cases where there are transmissions
  df_04        = melt(matrix_02, id.vars = c("combined_data$Patient", "combined_data$Isolate", "combined_data$Unit"))
  names(df_04) = c("Patient.ID", "Isolate.ID", "Unit", "Variable", "Value")
  df_04        = subset(df_04, df_04$Value > 0)
  
  # Select the ID associated with your subset
  isolate_of_interest = subset(df_04, df_04$Isolate == subset_isolate)
  subset_id           = as.numeric(isolate_of_interest$Variable)
  
  df_05               = subset(df_04, df_04$Variable == subset_id)

  # Select the collection dates
  collection_dates    = unique(subset(df_01, select = c("Isolate.ID", "Collection.Date")))
  df_06 = merge(df_05, collection_dates, by = c("Isolate.ID"))
  
  
  
  ################# PLOT ######################
  
  
  
  # Plot Dataframe
  plot_data        = unique(subset(df_06, select = c("Isolate.ID", "Isolate.ID", "Collection.Date", "Unit", "Patient.ID")))
  names(plot_data) = c("id", "content", "start", "group", "title")
  
  # Groups Dataframe
  grou_data        = unique(subset(plot_data, select = c("group", "group")))
  names(grou_data) = c("id", "content")
  
  # Custom annotation for each box:
  annotation <- function(above, below) {
    sprintf(
      '<table><tbody>
      <tr>
      <td>%s</td>
      <th>&nbsp;</th>
      <tr>
      <tr>
      <td>%s</td>
      <th>&nbsp;</th>
      </tr>
      </tbody></table>',
      above,
      below
    )
  }
  
  plot_data$content = annotation(plot_data$title, plot_data$id)
  
  
  
  ################# PLOT ######################
  
  
  
  return(timevis(data = plot_data, groups = grou_data, options = list(editable = TRUE)))
  
}

  