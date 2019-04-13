function_plot_persi_4 = function(input_data, subset_pathogen, subset_procedure, subset_patient, subset_agent){
  
  
  
  ################# LOAD DATA ######################
  


  df_01 = input_data

  
  
  
  ################# SUBSET ######################
  
  
  
  df_01 = subset(df_01, df_01$Pathogen %in% subset_pathogen)
  
  if(subset_procedure == "All"){df_01 = df_01}
  if(subset_procedure != "All"){df_01 = subset(df_01, df_01$Procedure %in% subset_procedure)}
  
  df_01 = subset(df_01, df_01$Patient.ID %in% subset_patient)
  df_01 = subset(df_01, df_01$Agent %in% subset_agent)

  df_01$Collection.Date = as.Date(df_01$Collection.Date)
  df_01$MIC_Clean       = as.numeric(as.character(df_01$MIC_Clean))
  
  
  
  ################# PLOT ######################
  
  
  
  plot_data = subset(df_01, select = c("Isolate.ID", "Procedure", "Collection.Date", "Agent", "MIC_Clean", "Status"))

  plot_01 = ggplot(
    data = plot_data

  ) +
    
    geom_point(
      aes(
        x     = Collection.Date,
        y     = MIC_Clean,
        color = Agent,
        group = Agent,
        shape = Procedure,
        size  = 2
        
      )
    ) +

    geom_line(
      aes(
        x     = Collection.Date,
        y     = MIC_Clean,
        color = Agent,
        group = Agent,
        shape = Procedure

      )
    ) +
    
    theme(
      axis.title.x       = element_blank(),
      axis.line.x        = element_blank(),
      axis.line.y        = element_blank(),
      
      panel.background   = element_blank(),
      panel.grid.major.x = element_line(color = "gray"),
      panel.grid.major.y = element_line(color = "gray"),
      panel.border       = element_blank(),
      
      strip.text.x       = element_text(size = 6),
      strip.background   = element_rect(fill = "white"),
      
      legend.position    = "none"
      
    ) +
    
    ylab("MIC")
  
  plot_01 = ggplotly(plot_01)
  return(plot_01)
  
}