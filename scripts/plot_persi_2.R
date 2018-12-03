function_plot_persi_2 = function(input_data, subset_pathogen, subset_procedure){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data
  # subset_pathogen = "Staphylococcus aureus"
  # subset_procedure = "Culture-blood"
  
  
  
  ################# SUBSET ######################
  
  
  
  df_01 = subset(df_01, df_01$Pathogen %in% subset_pathogen)
  df_01 = subset(df_01, df_01$Procedure %in% subset_procedure)
  df_01 = subset(df_01, (df_01$TYPE == "Persistent") | (df_01$TYPE == "Single"))
  df_01$MIC_Clean = as.numeric(as.character(df_01$MIC_Clean))
  
  
  ################# CALCULATIONS ######################
  
  
  
  matrix_01 = do.call(
    data.frame,
    aggregate(
      formula = MIC_Clean ~ Pathogen + TYPE + Agent,
      data = df_01,
      FUN = mean
      
    )
  )
  
  
  
  
  ################# PLOT ######################
  
  
  
  plot_01 = ggplot(
    data = matrix_01,
    aes(
      x     = Agent,
      y     = MIC_Clean,
      fill = TYPE
      
    )
  ) +
    
    geom_bar(
      stat = "identity",
      position = "dodge"
    ) +
    
    facet_grid(
      
      . ~ Agent,
      scales = "free_x"
      
    ) +
    
    theme(
      axis.title.x       = element_blank(),
      axis.text.x        = element_blank(),
      axis.line.x        = element_blank(),
      axis.ticks.x       = element_blank(),
      axis.line.y        = element_blank(),
      
      panel.background   = element_blank(),
      panel.grid.major.y = element_line(color = "gray"),
      panel.border       = element_rect(colour = "black", fill = NA),
      
      strip.text.x       = element_text(size = 6),
      strip.background   = element_rect(fill = "white")
      
    ) +
    
    ylab("Mean MIC")
  
  
  plot_01 = ggplotly(plot_01)
  return(plot_01)
  
}