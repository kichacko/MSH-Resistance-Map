function_plot_persi_2 = function(input_data, subset_pathogen, subset_procedure){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data
  
  
  
  ################ SUBSET ######################
  
  
  
  # Subset
  df_01 = subset(df_01, df_01$Pathogen %in% subset_pathogen)
  
  if(subset_procedure == "All"){df_01 = df_01}
  if(subset_procedure != "All"){df_01 = subset(df_01, df_01$Procedure %in% subset_procedure)}
  
  
  # Assign Types and Remove overlapping patients
  sin = subset(df_01, (df_01$B == 0) & (df_01$G == 0))
  per = subset(df_01, (df_01$C == 1) & (df_01$G == 0))
  het = subset(df_01, (df_01$F == 1) | (df_01$F == -1) & (df_01$G == 0))
  
  patients = unique(subset(het, select = c("Patient.ID")))
  per      = subset(per, !(per$Patient.ID %in% patients$Patient.ID))
  
  sin$Type = "Single"
  per$Type = "Persistent"
  het$Type = "Heteroresistant"
  
  df_01 = rbind(sin, per)
  df_01 = rbind(df_01, het)
  
  df_01 = subset(df_01, select = c("Patient.ID", "Pathogen", "Agent", "Year", "F", "Type"))
  df_01 = subset(df_01, df_01$Type == "Heteroresistant")
  df_01 = unique(df_01)
  
  
  
  ################# CALCULATIONS ######################
  
  
  
  # Matrix
  matrix_01 = dcast(
    data = df_01,
    formula = Agent ~ F,
    fun = sum,
    value.var = "F"
    
  )
  
  matrix_01        = melt(matrix_01, id.vars = "Agent")
  names(matrix_01) = c("Agent", "Direction", "Count")
  matrix_01$Count  = as.numeric(as.character(matrix_01$Count))
  
  matrix_01$Type[matrix_01$Direction ==  "1"] = "Gain"
  matrix_01$Type[matrix_01$Direction == "-1"] = "Loss"
  
  
  
  ################# PLOT ######################
  
  
  plot_data = matrix_01
  
  
  plot_01 = ggplot(
    data = plot_data

  ) +
    
    geom_bar(
      aes(
        x    = Agent,
        y    = Count,
        fill = Type,
        text = paste("Direction: ", Type, "<br>Count: ", Count) 
      ), 
      
      stat = 'identity',
      position = 'stack',
      color = 'black'
      
    ) +
    
    theme(
      axis.title.x       = element_blank(),
      axis.line.x        = element_blank(),
      axis.text.x        = element_text(angle = 90, size = 6),
      axis.title.y       = element_blank(),
      axis.line.y        = element_blank(),
      
      panel.background   = element_blank(),
      panel.grid.major.x = element_line(color = "gray"),
      panel.grid.major.y = element_blank(),
      
      legend.position    = "none"
      
    ) +
    
    ylab("Number of cases")

  
  plot_01 = ggplotly(plot_01, tooltip = c("x", "y", "Direction"), pdf(file = NULL))
  return(plot_01)
  
}
        
