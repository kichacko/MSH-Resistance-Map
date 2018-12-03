function_plot_resis_2 = function(input_data, subset_genome){

  

  ################# LOAD DATA ######################
  
  
  
  df_01 = resistome_data
  
  
  
  ################ SUBSET ######################
  
  
  
  # Subset
  df_01 = subset(df_01, df_01$Isolate.ID %in% subset_genome)
  df_01$Count = 1
  
    
    
  ################# MATRIX ######################
  
  
  
  # Matrix
  matrix_01 = dcast(
    data          = df_01,
    formula       = `Isolate.ID` ~ `Gene`,
    value.var     = "Count",
    fun.aggregate = sum
    
  )
  
  
  colnames(matrix_01)[1]   = "Isolate.ID"
  matrix_01                = as.data.frame(matrix_01)
  # matrix_01              = unique(matrix_01)
  # matrix_01$ID           = paste("Profile", rownames(matrix_01), sep = " ")
  rownames(matrix_01)      = matrix_01$`Isolate.ID`
  matrix_01[1]             = NULL
  
  
  
  ################# PLOT ######################
  
  
  
  plot_01 = heatmaply(matrix_01,
            scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(low = "white", high = "steelblue"),
            showticklabels = c(TRUE, TRUE),
            hide_colorbar = TRUE,
            grid_gap = 1,
            grid_color = "black",
            branches_lwd = 0.50,
            margins = c(50,5,NA,10)
  )
  
  return(plot_01)
  
}
