function_plot_trans_1 = function(input_data, subset_pathogen, subset_procedure, subset_year, subset_size){
  
  
  
  ################# LOAD DATA ######################
  
  
  
  df_01       = input_data
  subset_year = as.numeric(as.character(subset_year))
  subset_size = as.numeric(as.character(subset_size))
  
  
  
  
  ################# SUBSET ######################
  
  
  
  # Subset
  df_01 = subset(df_01, df_01$Pathogen %in% subset_pathogen)
  df_01 = subset(df_01, df_01$Procedure %in% subset_procedure)
  df_01 = df_01[(df_01$Year     == subset_year) | (df_01$Year      == (subset_year - 1)) , ]
  df_01 = subset(df_01, select = c("Patient.ID", "Isolate.ID", "Pathogen", "Agent", "MIC", "Year", "Unit"))
  df_01 = unique(df_01)
  
  
  
  ################# ID ######################
  
  
  
  # Create custom ID
  df_01 = transform(
    df_01,
    ID = match(
      df_01$`Patient.ID`,
      unique(df_01$`Patient.ID`)
      
    )
  )
  
  
  
  ################# MATRIX ######################
  
  
  
  
  # matrix_01
  matrix_01 = dcast(
    data          = df_01,
    formula       = df_01$Patient.ID + df_01$Isolate.ID + df_01$Unit ~ df_01$Agent + df_01$MIC + df_01$Unit,
    value.var     = "MIC",
    fun.aggregate = length
    
  )
  
  colnames(matrix_01)[1] = "Patient"
  colnames(matrix_01)[2] = "Isolate"
  colnames(matrix_01)[3] = "Unit"
  
  
  
  ################# CALCULATIONS ######################
  
  
  
  # Create a "Unique data" frame that identifies the unique resistance profiles, sorts them by a pseudo intensitiy, and creates a unique ID for each
  unique_data         = matrix_01
  unique_data[1:3]    = NULL
  unique_data         = unique(unique_data)
  unique_data$ColSum  = rowSums(unique_data)
  unique_data         = unique_data[order(unique_data[["ColSum"]]),]
  unique_data$ID      = 1:nrow(unique_data)
  
  # Match these IDs back to the original matrix_01
  combined_data = merge(unique_data, matrix_01)
  combined_data = combined_data[row.names(unique(combined_data[,c("ID", "Patient")])),]  # So you can remove isolates from patients with the same ID
  combined_data = subset(combined_data, select = c("ID", "Patient", "Isolate", "Unit"))
  
  
  matrix_02 = dcast(
    data = combined_data,
    formula = combined_data$Patient + combined_data$Isolate + combined_data$Unit ~ combined_data$ID,
    value.var = "ID",
    fun.aggregate = length
    
  )
  
  
  # Filter for cases where there are transmissions
  filter_function <- function(x){
    if(
      is.numeric(x)){
      sum(x) >= subset_size
      
    }
    else {
      TRUE
      
    }
  }
  
  matrix_02 = matrix_02[, sapply(matrix_02,  filter_function)]
  
  
  
  ################# CLEAN ######################
  
  
  
  plot_data        = melt(matrix_02, id.vars = c("combined_data$Patient", "combined_data$Isolate", "combined_data$Unit"))
  names(plot_data) = c("Patient", "Isolate", "Unit", "Variable", "Value")
  plot_data        = subset(plot_data, plot_data$Value > 0)
  
  
  
  ################# PLOT ######################
  
  
  
  
  plot_01 = ggplot(
    plot_data,
    aes(
      x = Isolate,
      y = Variable,
      text = paste(Unit, Patient)
      
    )
  ) +
    
    geom_tile(
      aes(
        fill = as.numeric(Unit)
        
      ),
      colour = "white"
      
    ) + 
    
    scale_fill_gradient(
      low = "gold",
      high = "steelblue"
      
    ) +
    
    theme(
      axis.text.x        = element_blank(),
      axis.line.x        = element_blank(),
      axis.line.y        = element_blank(),
      
      panel.background   = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "gray"),
      
      strip.text.x       = element_text(size = 6),
      strip.background   = element_rect(fill = "white"),
      
      legend.position    = "none"
      
    ) +
    
    labs(
      x = "Isolate",
      y = "Transmission ID"
      
    )
  
  plot_01 = ggplotly(plot_01, tooltip = c("x", "text"), pdf(file = NULL))
  
  return(plot_01)
  
}