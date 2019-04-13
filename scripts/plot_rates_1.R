function_plot_rates_1 = function(input_data, subset_pathogen, subset_procedure, subset_type, subset_func){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data

  
  
  ################# SUBSET ######################
  
  
  
  # Subset
  df_01 = subset(df_01, df_01$Pathogen %in% subset_pathogen)
  
  if(subset_type == "All")            {df_01 = df_01}
  if(subset_type == "Case")           {df_01 = subset(df_01, df_01$A == 1 & df_01$G == 0)}
  if(subset_type == "Single")         {df_01 = subset(df_01, df_01$B == 0 & df_01$G == 0)}
  if(subset_type == "Persistent")     {df_01 = subset(df_01, df_01$C == 1 & df_01$G == 0)}
  if(subset_type == "Polymicrobial")  {df_01 = subset(df_01, df_01$G == 1)}
  if(subset_type == "Heteroresistant"){df_01 = subset(df_01, df_01$F == 0 | df_01$F == 1 | df_01 == -1)}
  
  if(subset_procedure == "All"){df_01 = df_01}
  if(subset_procedure != "All"){df_01 = subset(df_01, df_01$Procedure %in% subset_procedure)}
  
  
  
  
  ################# CONDITIONALS ######################
  
  
  
  if(subset_func == "Cases"){
    x = "sum"
    df_01 = subset(df_01, df_01$Numeric.Status == 1)
    
  }
  
  if(subset_func == "Percentages"){
    x = "mean"
    df_01 = df_01
    
  }
  
  
  
  ################# CALCULATIONS ######################
  
  
  
  matrix_01 = do.call(
    data.frame,
    aggregate(
      formula = Numeric.Status ~ Pathogen + Agent + Year,
      data = df_01,
      FUN = x
      
    )
  )
  
  names(matrix_01) = c("Pathogen", "Agent", "Year", "Count")
  
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
      fill  = Agent,
      color = Agent,
      group = 1
      
    )
  ) +
    
    geom_point() +
    
    geom_line() +
    
    facet_grid(. ~ Agent) +
    
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
    
    ylab("Count or Rate") +
    
    scale_x_continuous(limits = c(2014, 2018), breaks = seq(2014, 2018, by = 1))
  
  graphics.off()
  
  plot_01 = ggplotly(plot_01, tooltip = c("x", "y", "fill"), pdf(file = NULL))
  return(plot_01)
  
}