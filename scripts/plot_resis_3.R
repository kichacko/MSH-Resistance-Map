function_plot_resis_3 = function(input_data, subset_genome){

  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data
  
  
  
  ################ SUBSET ######################
  
  
  
  # Subset
  df_01 = subset(df_01, df_01$Isolate.ID %in% subset_genome)
  
  
  
  ################# DATA TABLE ######################
  
  table_01 = df_01
  table_01 = subset(table_01, select = c("Isolate.ID", "Gene", "Homolog...Variant", "Agent", "Contig", "Start", "Stop", "PROKKA.ID" ,"PROKKA.Description"))
  
  return(table_01)
  
}