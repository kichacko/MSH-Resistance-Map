function_plot_persi_4 = function(input_data, subset_pathogen){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data
  
  
  
  ################ SUBSET ######################
  
  
  
  
  # Subset
  df_01 = subset(df_01, df_01$Pathogen == subset_pathogen)
  df_01 = subset(df_01, (df_01$C == -1) | (df_01$C == 1))
  
  
  
  
  ################# MATRIX ######################
  
  
  
  
  # Create Matrix
  matrix = dcast(
    data = df_01,
    formula = df_01$Agent ~ df_01$Flip_Time,
    fun.aggregate = length,
    value.var = 'Flip_Time'
    
  )
  
  # Create output
  df_02 = matrix
  for (i in seq_len(nrow(df_02))){
    for (j in seq_len(ncol(df_02))){
  
      total = rowSums( matrix[ sapply(matrix[i, ], is.numeric)] )[i]
  
      if ( (is.numeric(matrix[i, j]) == TRUE) & (is.numeric(matrix[i, j - 1]) == FALSE)) {
        df_02[i, j] = as.numeric(total - matrix[i, j])
  
      }
  
      if ( is.numeric(matrix[i, j]) == TRUE & (is.numeric(matrix[i, j - 1]) == TRUE) & (is.numeric(matrix[i, j + 1]) == TRUE)) {
        df_02[i, j] = as.numeric(df_02[i, j - 1] - matrix[i, j])
  
      }
  
      else if (j == ncol(matrix)) {
        df_02[i, j] = as.numeric(df_02[i, j - 1] - matrix[i, j])
  
      }
    }
  }
  
  # Convert to percentages
  df_03 = df_02
  for (i in 1:nrow(df_02)){
    max = max(as.numeric(df_02[ , sapply(df_02, is.numeric)][i,]))
  
    for (j in 1:ncol(df_02)){
      if ( is.numeric(df_02[i, j]) == TRUE){
        df_03[i, j] = df_02[i, j] / max
  
      }
    }
  }
  
  
  # Convert Back to Dataframe
  plot_data = melt(df_03, id.vars=c("df_01$Agent"))
  names(plot_data) <- c("Agent", "Days", "Proportion")
  plot_data$Days = as.numeric(as.character(plot_data$Days))
  plot_data = as.data.frame(plot_data)
  
  
  ################# PLOT ######################
  
  
  
  
  
  # Plot
  plot     = ggplot(
    data   = plot_data
  
    ) +
  
    geom_step(
      data    = plot_data,
      aes(
        x     = Days,
        y     = Proportion,
        color = Agent
  
      )
    ) +
    
    theme(
      axis.title.x       = element_blank(),
      axis.line.x        = element_blank(),
      axis.title.y       = element_blank(),
      axis.line.y        = element_blank(),
      
      panel.background   = element_blank(),
      panel.grid.major.x = element_line(color = "gray"),
      
      legend.position    = "none"
      
    )
  
  plot = ggplotly(plot, pdf(file = NULL))
  return(plot)
  
}
