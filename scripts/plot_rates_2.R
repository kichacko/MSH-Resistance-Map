function_plot_rates_2 = function(input_data, subset_pathogen, subset_procedure, subset_agent, subset_year, subset_func){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data
  subset_year = as.numeric(as.character(subset_year))
  
  
  
  ################# SUBSET ######################
  
  
  
  # Subset
  df_01 = df_01[df_01$A              == 1, ]
  df_01 = subset(df_01, df_01$Pathogen %in% subset_pathogen)
  
  if(subset_procedure == "All"){
    df_01 = df_01
    
  }
  
  if(subset_procedure != "All"){
    df_01 = subset(df_01, df_01$Procedure %in% subset_procedure)
    
  }
  
  df_01 = subset(df_01, df_01$Agent %in% subset_agent)
  df_01 = subset(df_01, df_01$Year %in% subset_year)

  
  
  ################# CONDITIONALS ######################
  
  
  
  if(subset_func == "Cases"){
    x = "sum"
    df_01 = df_01[df_01$Numeric.Status == 1, ]
    
  }
  
  if(subset_func == "Percentages"){
    x = "mean"
    df_01 = df_01
    
  }
  
  
  
  ################# CALCULATIONS ######################
  
  
  
  plot_data = do.call(
    data.frame,
    aggregate(
      formula = Numeric.Status ~ Pathogen + Agent + Month,
      data = df_01,
      FUN = x
      
    )
  )
  
  names(plot_data) = c("Pathogen", "Agent", "Month", "Count")
  
  plot_data$Count   = as.numeric(as.character(plot_data$Count))
  plot_data$Month  = as.numeric(as.character(plot_data$Month))
  
  
  
  ################# PLOT ######################
  
  
  
  # Plot
  plot = ggplot(
    data = plot_data,
    aes(
      x     = Month,
      y     = Count,
      fill  = Agent,
      color = Agent,
      group = 1
      
    )
  ) +
    
    geom_point() +
    
    geom_line() +
    
    theme(
      axis.line.x        = element_blank(),
      axis.line.y        = element_blank(),
      
      panel.background   = element_blank(),
      panel.grid.major.y = element_line(color = "gray"),
      panel.border       = element_blank(),
      
      strip.text.x       = element_text(size = 6),
      strip.background   = element_rect(fill = "white"),
      
      legend.position    = "none"
      
    ) +
    
    labs(
      x = "Month",
      y = "Count or Rate"
      
    ) +
    
    scale_x_continuous(limits = c(1, 12), breaks = seq(1, 12, by = 1)) +
    
    ylim(0, max(plot_data$Count))
  
  graphics.off()
  
  plot = ggplotly(plot, tooltip = c("x", "y", "fill"), pdf(file = NULL))
  return(plot)
  
}
