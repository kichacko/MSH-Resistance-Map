function_plot_summ_1 = function(input_data, subset_pathogen, subset_procedure){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data
  
  
  
  ################# SUBSET ######################
  
  
  
  # Subset
  df_01 = subset(df_01, df_01$Pathogen %in% subset_pathogen)
  df_01 = subset(df_01, df_01$Procedure %in% subset_procedure)
  df_01 = df_01[row.names(unique(df_01[ ,c("Isolate.ID", "Pathogen", "Procedure", "Year")])),]
  
  
  ################# CALCULATIONS ######################
  
  matrix_01 = do.call(
    data.frame,
    aggregate(
      formula = Isolate.ID ~ Pathogen + Procedure + Year,
      data = df_01,
      FUN = length
      
    )
  )
  
  names(matrix_01) = c("Pathogen", "Procedure", "Year", "Count")
  
  matrix_01$Count  = as.numeric(as.character(matrix_01$Count))
  matrix_01$Year   = as.numeric(as.character(matrix_01$Year))
  
  
  
  ################# PLOT ######################
  
  
  
  plot_data = matrix_01
  
  
  # Plot
  plot_01 = ggplot(
    data = plot_data,
    aes(
      x     = Year,
      y     = Count,
      fill  = Pathogen,
      color = Pathogen,
      group = 1
      
    )
  ) +
    
    geom_point() +
    
    geom_line() +
    
    theme(
      axis.title.x       = element_blank(),
      axis.text.x        = element_text(angle = 90, size = 6),
      axis.line.x        = element_blank(),
      axis.line.y        = element_blank(),
      
      panel.background   = element_blank(),
      panel.grid.major.y = element_line(color = "gray"),
      panel.border       = element_rect(colour = "black", fill = NA),
      
      strip.text.x       = element_text(size = 6),
      strip.background   = element_rect(fill = "white"),
      
      legend.position    = "none"
      
    ) +
    
    ylab("Number of isolates") +
    
    scale_x_continuous(limits = c(2014, 2018), breaks = seq(2014, 2018, by = 1))
  
  graphics.off()
  
  plot_01 = ggplotly(plot_01, tooltip = c("x", "y", "fill"), pdf(file = NULL))
  return(plot_01)
  
}