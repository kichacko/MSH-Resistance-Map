function_plot_rates_4 = function(input_data, subset_pathogen, subset_procedure, subset_agent, subset_year){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data
  subset_year = as.numeric(as.character(subset_year))
  
  
  
  ################ SUBSET ######################
  
  
  
  # Subset
  df_01 = df_01[df_01$A         == 1, ]
  df_01 = subset(df_01, df_01$Pathogen %in% subset_pathogen)
  df_01 = subset(df_01, df_01$Procedure %in% subset_procedure)
  df_01 = subset(df_01, df_01$Agent %in% subset_agent)
  df_01 = subset(df_01, df_01$Year %in% subset_year)
  
  
  
  ################# DATA TABLE ######################
  
  table_01 = df_01
  table_01 = subset(table_01, select = c("Isolate.ID", "Patient.ID", "Collection.Date", "Unit", "Procedure", "Pathogen", "Agent", "MIC", "Status"))
  
  return(table_01)
  
}