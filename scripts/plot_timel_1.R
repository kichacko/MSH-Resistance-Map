function_plot_timel_1 = function(input_data, subset_patient){
  


  ################# LOAD DATA ######################
  
  
  
  df_01 = input_data
  subset_patient = as.character(subset_patient)
  # subset_patient = "1470"
  
  
  
  ################# SUBSET ######################
  
  
  # Data
  df_01        = subset(df_01, df_01$Patient.ID %in% subset_patient)
  df_01        = unique(subset(df_01, select = c("Isolate.ID", "Isolate.ID", "Collection.Date", "Pathogen", "Procedure")))
  names(df_01) = c("id", "content", "start", "group", "title")
  # df_01$type   = "point"
  
  # Groups Dataframe
  df_02 = unique(subset(df_01, select = c("group", "group")))
  names(df_02) = c("id", "content")
  
  
  
  ################# CALCULATIONS ######################
  


  # Custom annotation for each box:
  annotation <- function(procedure, isolate) {
    sprintf(
      '<table><tbody>
      <tr>
        <td>%s</td>
        <th>&nbsp;</th>
      <tr>
      <tr>
        <td>%s</td>
        <th>&nbsp;</th>
      </tr>
      </tbody></table>',
      procedure,
      isolate
    )
  }
  
  df_01$content = annotation(df_01$title, df_01$id)



  ################# PLOT ######################
  
  
  
  return(timevis(data = df_01, groups = df_02, options = list(editable = TRUE)))
  
}
  