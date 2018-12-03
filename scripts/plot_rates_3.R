function_plot_rates_3 = function(input_data, subset_pathogen, subset_procedure, subset_agent, subset_year, subset_func){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data
  subset_year = as.numeric(as.character(subset_year))
  
  
  
  ################# SUBSET ######################
  
  
  
  # Subset
  df_01 = df_01[df_01$A         == 1, ]
  df_01 = subset(df_01, df_01$Pathogen %in% subset_pathogen)
  df_01 = subset(df_01, df_01$Procedure %in% subset_procedure)
  df_01 = subset(df_01, df_01$Agent %in% subset_agent)
  df_01 = df_01[(df_01$Year     == subset_year) |
                (df_01$Year     == subset_year - 1), ]
  
  df_01 = subset(df_01, select = c("Patient.ID", "Pathogen", "Agent", "Unit", "Year", "A", "B", "Numeric.Status"))
  df_01$Count = 1
  df_01 = unique(df_01)
  
  
  
  ################# CALCULATIONS ######################
  
  
  
  # Matrix
  matrix_01 = dcast(
    data      = df_01,
    formula   = df_01$Unit ~ df_01$Year,
    fun       = sum,
    value.var = "Count"
    
  )
  
  names(matrix_01) = c("Unit", "Count_Previous_Year", "Count_Selected_Year")
  
  matrix_02 = dcast(
    data      = df_01,
    formula   = df_01$Unit ~ df_01$Year,
    fun       = mean,
    value.var = "Numeric.Status"
    
  )
  
  names(matrix_02) = c("Unit", "Previous_Year", "Selected_Year")
  
  matrix_03 = merge(matrix_01, matrix_02)
  matrix_03 = na.omit(matrix_03)
  
  matrix_03$Previous_Year = as.numeric(as.character(matrix_03$Previous_Year))
  matrix_03$Selected_Year = as.numeric(as.character(matrix_03$Selected_Year))

  matrix_03 = subset(matrix_03,
                     (matrix_03$Count_Previous_Year > (.25 * max(matrix_03$Count_Previous_Year))) |
                       (matrix_03$Count_Selected_Year > (.25 * max(matrix_03$Count_Selected_Year)))
                     
  )
  
  
  
  ################# CONDITIONALS ######################
  
  
  
  if(subset_func == "Cases"){
    
    plot_data = subset(matrix_03, select = c("Unit", "Count_Previous_Year", "Count_Selected_Year"))
    names(plot_data) = c("Unit", "Previous_Year", "Selected_Year")
    
  }
  
  if(subset_func == "Percentages"){
    
    plot_data = subset(matrix_03, select = c("Unit", "Previous_Year", "Selected_Year"))
    names(plot_data) = c("Unit", "Previous_Year", "Selected_Year")
    
  }
  
  

  ################# PLOT ######################
  
  
  
  plot_01 = ggplot(
    data = plot_data
    
  ) +
    
    geom_segment(
      aes(
        x = reorder(Unit, -Selected_Year),
        xend = reorder(Unit, -Selected_Year),
        y = Previous_Year,
        yend = Selected_Year
        
      ),
      color = "grey"
      
    ) +
    geom_point(
      aes(
        x = Unit,
        y = Previous_Year,
        text = paste("Year: ", subset_year - 1, "<br>Count or Rate: ", Previous_Year, "<br>Unit: ", Unit)
        
      ),
      color = "steelblue",
      size = 3
      
    ) +
    geom_point(
      aes(
        x = Unit,
        y = Selected_Year,
        text = paste("Year: ", subset_year, "<br>Count or Rate: ", Selected_Year, "<br>Unit: ", Unit)
        
      ), 
      color = "gold",
      size = 3
      
    ) +
    
    theme(
      axis.text.x  = element_text(angle = 90),
      
      panel.background   = element_blank(),
      panel.grid.major.y = element_line(color = "gray")
      
    ) +
    
    labs(
      x = "Unit",
      y = "Count or Rate"
      
    )
  
  plot_01 = ggplotly(plot_01, tooltip = c("text"), pdf(file = NULL))
  return(plot_01)
}
