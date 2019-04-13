function_plot_summ_2 = function(input_data, subset_pathogen, subset_procedure, subset_year){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data
  subset_year = as.numeric(as.character(subset_year))
  
  
  
  ################# SUBSET ######################
  
  
  
  # Subset
  df_01 = subset(df_01, df_01$Pathogen %in% subset_pathogen)
  df_01 = subset(df_01, df_01$Procedure %in% subset_procedure)
  df_01 = df_01[df_01$Year == subset_year, ]
  df_01 = df_01[row.names(unique(df_01[ , c("Isolate.ID", "Pathogen", "Procedure", "Year")])),]
  
  
  ################# CALCULATIONS ######################
  
  matrix_01 = do.call(
    data.frame,
    aggregate(
      formula = Isolate.ID ~ Pathogen + Procedure + Unit,
      data = df_01,
      FUN = length
      
    )
  )
  
  names(matrix_01) = c("Pathogen", "Procedure", "Unit", "Count")
  
  matrix_01$Count  = as.numeric(as.character(matrix_01$Count))
  
  
  
  ################# PLOT ######################
  
  
  
  # plot
  plot_01  = ggplot(
    data   = matrix_01,
    aes(
      x    = Unit,
      y    = Count,
      fill = Unit
      
    )
  ) +
    
    geom_bar(
      position = "dodge",
      stat     = "identity",
      color    = "white"
      
    ) +
    
    theme(
      axis.title.x     = element_blank(),
      axis.text.x      = element_text(size = 6, angle = 90),
      axis.line.x      = element_line(color="black"),
      axis.line.y      = element_line(color="black"),
      
      panel.background = element_blank(),
      panel.grid       = element_line(color = "gray"),
      
      strip.text.x     = element_text(size = 8),
      
      legend.position  = "none"
      
    ) +
    
    ylab("Number of isolates")
  
  plot_01 = ggplotly(plot_01, tooltip = c("x", "fill","y"), pdf(file = NULL))
  return(plot_01)
  
}