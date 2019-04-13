function_plot_persi_1 = function(input_data, subset_pathogen, subset_procedure){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data

  
  
  ################# SUBSET ######################

  
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
  
  df_01 = subset(df_01, select = c("Patient.ID", "Pathogen", "Year", "Type"))
  df_01 = unique(df_01)
  df_01$Count = 1
  
  
  
  ################# CALCULATIONS ######################
  
  
  
  matrix_01 = do.call(
    data.frame,
    aggregate(
      formula = Count ~ Pathogen + Year + Type,
      data = df_01,
      FUN = sum
      
    )
  )
  
  
  
  ################# PLOT ######################
  
  
  
  plot_data = matrix_01
  
  plot_01 = ggplot(
    data = plot_data
      
  ) +
    
    geom_bar(
      aes(
        x    = Year,
        y    = Count,
        fill = Type,
        text = paste("Year: ", Year, "<br>Type: ", Type, "<br>Count: ", Count)
        
      ),
      position = "fill",
      stat     = "identity",
      color    = 'black'
      
    ) +
    
    theme(
      axis.title.x       = element_blank(),
      axis.text.x        = element_text(angle = 90, size = 6),
      axis.line.x        = element_blank(),
      axis.line.y        = element_blank(),
      
      panel.background   = element_blank(),
      panel.grid.major.y = element_line(color = "gray"),
      panel.border       = element_blank(),
      
      strip.text.x       = element_text(size = 6),
      strip.background   = element_rect(fill = "white"),
      
      legend.position    = "none"
      
    ) +
    
    ylab("Percent")
    
    
  
  plot_01 = ggplotly(plot_01, tooltip = "text", pdf(file = NULL))
  return(plot_01)
  
}
  
  
  