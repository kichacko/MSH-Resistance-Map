function_plot_persi_1 = function(input_data, subset_pathogen, subset_procedure){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = summary_data

  
  
  ################# SUBSET ######################


  
  df_01$Count = 1
  df_01 = subset(df_01, df_01$Pathogen %in% subset_pathogen)
  df_01 = subset(df_01, df_01$Procedure %in% subset_procedure)
  df_01 = unique(subset(df_01, select = c("Patient.ID", "Pathogen", "Procedure", "Year", "TYPE", "Count")))
  
  
  
  ################# CALCULATIONS ######################
  
  
  
  matrix_01 = dcast(
    data = df_01,
    formula = df_01$Pathogen + df_01$Procedure + df_01$Year ~ df_01$TYPE,
    value.var = "Count",
    fun.aggregate = sum
    
  )


  matrix_01$Heteroresistant = as.numeric(as.character(matrix_01$Heteroresistant))
  matrix_01$Persistent = as.numeric(as.character(matrix_01$Persistent))
  matrix_01$Single = as.numeric(as.character(matrix_01$Single))
  
  names(matrix_01) = c("Pathogen", "Procedure", "Year", "Heteroresistant", "Persistent", "Single")
  
  matrix_01$Total           = matrix_01$Heteroresistant + matrix_01$Persistent + matrix_01$Single
  matrix_01$Heteroresistant = (matrix_01$Heteroresistant / matrix_01$Total) * 100
  matrix_01$Persistent      = (matrix_01$Persistent / matrix_01$Total) * 100
  matrix_01$Single          = (matrix_01$Single / matrix_01$Total) * 100
  
  matrix_01 = subset(matrix_01, select = c("Pathogen", "Procedure", "Year", "Heteroresistant", "Persistent", "Single"))
  
  
  ################# PLOT ######################
  
  
  
  plot_data = melt(matrix_01, id.vars = c("Pathogen", "Procedure", "Year"))
  
  plot_01 = ggplot(
    data = plot_data,
    aes(
      x = Year,
      y = value,
      fill = variable,
      text = paste("Year: ", Year, "<br>Percent: ", value, "<br>Type: ", variable)
      
    )
  ) +
    
    geom_bar(
      stat = "identity",
      position = "stack"
      
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
      strip.background   = element_rect(fill = "white")
      
    ) +
    
    ylab("Percent")
    
    
  
  plot_01 = ggplotly(plot_01, tooltip = "text", pdf(file = NULL))
  return(plot_01)
  
}
  
  
  