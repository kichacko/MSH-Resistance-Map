function_plot_resis_1 = function(input_data){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data
  
  
  
  ################ SUBSET ######################
  
  
  
  # Subset
  df_01$Count = 1
  
  
  
  ################# MATRIX ######################
  
  
  
  # Matrix
  matrix_01 = dcast(
    data          = df_01,
    formula       = `Resistome.ID` + `Feature` + `Agent` + `Gene` ~ `Count`,
    value.var     = "Count",
    fun.aggregate = sum
    
  )
  
  
  colnames(matrix_01)[1] = "Resistome.ID"
  colnames(matrix_01)[2] = "Feature"
  colnames(matrix_01)[3] = "Agent"
  colnames(matrix_01)[4] = "Gene"
  
  plot_data = melt(data = matrix_01, id.vars = c("Resistome.ID", "Feature","Agent", "Gene"))
  
  
  ################# PLOT ######################
  
  
  plot = ggplot(
    data = plot_data,
    aes(
      x = Gene,
      y = value,
      fill = Agent,
      text = paste("Gene: ", Gene, "<br> Agent: ", Agent, "<br>Count: ", value))
    
    ) +
    
    facet_grid(
      Feature ~ .
      
    ) +
    
    geom_bar(stat = "identity") +
    
    theme(
      axis.line.x        = element_blank(),
      axis.title.x       = element_blank(),
      axis.text.x        = element_text(angle = 90),
      axis.line.y        = element_blank(),
      
      panel.background   = element_blank(),
      panel.grid.major.y = element_line(color = "gray"),
      panel.border = element_rect(colour = "black", fill = NA),
      
      strip.background = element_blank(),
      
      legend.position    = "none"
      
    ) +
    
    labs(
      y = "Count"
      
    )
  
  plot = ggplotly(plot, tooltip = "text", pdf(file = NULL))
  return(plot)
  
}