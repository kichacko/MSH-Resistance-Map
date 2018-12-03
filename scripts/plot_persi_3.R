function_plot_persi_3 = function(input_data, subset_variable){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data
  
  
  
  ################ SUBSET ######################
  
  
  
  # Subset
  df_01 = subset(df_01, df_01$Pathogen == subset_variable)
  df_01 = subset(df_01, (df_01$C == -1) | (df_01$C == 1))
  
  
  
  ################# MATRIX ######################
  
  
  
  # Matrix
  matrix_01 = dcast(
    data = df_01,
    formula = df_01$Agent ~ df_01$C,
    fun = sum,
    value.var = "C"
    
  )
  
  plot_data        = melt(matrix_01, id.vars = "df_01$Agent")
  names(plot_data) = c("Agent", "Direction", "Count")
  plot_data$Count  = as.numeric(as.character(plot_data$Count))
  
  
  
  
  ################# PLOT ######################
  
  
  
  
  plot_01 = ggplot(
    data = plot_data,
    aes(
      x = Agent,
      y = Count,
      fill = Direction
    )
  ) +
    
    geom_bar(
      stat = 'identity',
      position = 'stack'
    ) +
    
    theme(
      axis.title.x       = element_blank(),
      axis.line.x        = element_blank(),
      axis.text.x        = element_text(angle = 90, size = 6),
      axis.title.y       = element_blank(),
      axis.line.y        = element_blank(),
      
      panel.background   = element_blank(),
      panel.grid.major.x = element_line(color = "gray"),
      
      legend.position    = "none"
      
    )
  
  
  plot_01 = ggplotly(plot_01, tooltip = c("x", "y", "Direction"), pdf(file = NULL))
  return(plot_01)
  
}
        
