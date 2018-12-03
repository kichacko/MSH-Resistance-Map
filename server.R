# Load the Libraries
library(heatmaply)
library(reshape2)
library(timevis)
library(ggplot2)
library(plotly)
library(shiny)

shinyServer(
  function(input, output) {
    
    # Load Summary Data
    summary_data   = read.csv("./data/summary_data.csv", "\t", header = TRUE)
    resistome_data = read.csv("./data/resistome_data.csv", "\t", header = TRUE)
    
    # Reactive Elements
    pathogen_list                 = as.list(unique(sort(summary_data[["Pathogen"]])))
    output$choose_rates_pathogen  = renderUI({ selectInput("rates_pathogen", "Select Pathogen:", pathogen_list, selected = "Staphylococcus aureus");  })
    output$choose_trans_pathogen  = renderUI({ selectInput("trans_pathogen", "Select Pathogen:", pathogen_list, selected = "Staphylococcus aureus");  })
    output$choose_persi_pathogen  = renderUI({ selectInput("persi_pathogen", "Select Pathogen:", pathogen_list, selected = "Staphylococcus aureus");  })
    output$choose_summ_pathogen   = renderUI({ selectInput("summ_pathogen", "Select Pathogen:", pathogen_list, selected = "Staphylococcus aureus");  })
    
    procedure_list                = as.list(unique(sort(summary_data[["Procedure"]])))
    output$choose_rates_procedure = renderUI({ selectInput("rates_procedure", "Select Procedure:", procedure_list, selected = "Culture-blood");  })
    output$choose_trans_procedure = renderUI({ selectInput("trans_procedure", "Select Procedure:", procedure_list, selected = "Culture-blood");  })
    output$choose_persi_procedure = renderUI({ selectInput("persi_procedure", "Select Procedure:", procedure_list, selected = "Culture-blood");  })
    output$choose_summ_procedure  = renderUI({ selectInput("summ_procedure", "Select Procedure:", procedure_list, selected = "Culture-blood");  })
    
    agent_list                    = as.list(unique(sort(summary_data[["Agent"]])))
    output$choose_rates_agent     = renderUI({ selectInput("rates_agent", "Select Agent:", agent_list, selected = "Oxacillin", multiple = TRUE);  })
    output$choose_trans_agent     = renderUI({ selectInput("trans_agent", "Select Agent:", agent_list, selected = "Oxacillin", multiple = TRUE);  })
    output$choose_persi_agent     = renderUI({ selectInput("persi_agent", "Select Agent:", agent_list, selected = "Oxacillin", multiple = TRUE);  })
    
    year_list                     = as.list(unique(sort(summary_data[["Year"]])))
    output$choose_rates_year      = renderUI({ selectInput("rates_year", "Select Year:", year_list, selected = 2018);  })
    output$choose_trans_year      = renderUI({ selectInput("trans_year", "Select Year:", year_list, selected = 2018);  })
    output$choose_summ_year       = renderUI({ selectInput("summ_year", "Select Year:", year_list, selected = 2018);  })
    
    output$choose_rates_func      = renderUI({ radioButtons("rates_func", "Select Function:", list("# Resistant" = "Cases","% Resistant" = "Percentages"))  })
    
    isolate_list                  = as.list(unique(sort(summary_data[["Isolate.ID"]])))
    output$choose_trans_isolate   = renderUI({ textInput("trans_isolate", "Select Isolate:", value = "ER15238");  })
    output$choose_timel_isolate   = renderUI({ textInput("timel_isolate", "Select Isolate:", value = "ER15238");  })
    
    patient_list                  = as.list(unique(sort(summary_data[["Patient.ID"]])))
    output$choose_persi_patient   = renderUI({ selectInput("persi_patient", "Select Patient:", patient_list, selected = "1470");  })
    output$choose_timel_patient   = renderUI({ selectInput("timel_patient", "Select Patient:", patient_list, selected = "1470");  })
    
    species_list                  = as.list(c("Staphylococcus aureus"))
    output$choose_species         = renderUI({ selectInput("species", "Select Species:", species_list, selected = "Staphylococcus aureus");  })
        
    genome_list                   = as.list(unique(sort(resistome_data[["Isolate.ID"]])))
    output$choose_genome          = renderUI({ selectInput("genome", "Select Genome:", genome_list, selected = c("ER00385_3B_024847", "ER00767_3B_024153"), multiple = TRUE);  })
    
    output$choose_trans_size      = renderUI({sliderInput("trans_size", "Cluster Size:", min = 0, max = 10, value = 3);  })
    
    # Plots
    plot_rates_1_input  = source('./scripts/plot_rates_1.R', local = TRUE)$value
    output$plot_rates_1 = renderPlotly(function_plot_rates_1(summary_data, input$rates_pathogen, input$rates_procedure, input$rates_func))

    plot_rates_2_input  = source('./scripts/plot_rates_2.R', local = TRUE)$value
    output$plot_rates_2 = renderPlotly(function_plot_rates_2(summary_data, input$rates_pathogen, input$rates_procedure, input$rates_agent, input$rates_year, input$rates_func))

    plot_rates_3_input  = source('./scripts/plot_rates_3.R', local = TRUE)$value
    output$plot_rates_3 = renderPlotly(function_plot_rates_3(summary_data, input$rates_pathogen, input$rates_procedure, input$rates_agent, input$rates_year, input$rates_func))
    
    plot_persi_1_input  = source('./scripts/plot_persi_1.R', local = TRUE)$value
    output$plot_persi_1 = renderPlotly(function_plot_persi_1(summary_data, input$persi_pathogen, input$persi_procedure))
    
    plot_persi_2_input  = source('./scripts/plot_persi_2.R', local = TRUE)$value
    output$plot_persi_2 = renderPlotly(function_plot_persi_2(summary_data, input$persi_pathogen, input$persi_procedure))
    
    plot_persi_3_input  = source('./scripts/plot_persi_3.R', local = TRUE)$value
    output$plot_persi_3 = renderPlotly(function_plot_persi_3(summary_data, input$persi_pathogen))

    plot_persi_4_input  = source('./scripts/plot_persi_4.R', local = TRUE)$value
    output$plot_persi_4 = renderPlotly(function_plot_persi_4(summary_data, input$persi_pathogen))

    plot_persi_5_input  = source('./scripts/plot_persi_5.R', local = TRUE)$value
    output$plot_persi_5 = renderPlotly(function_plot_persi_5(summary_data, input$persi_pathogen, input$persi_procedure, input$persi_patient, input$persi_agent))
        
    plot_rates_4_input  = source('./scripts/plot_rates_4.R', local = TRUE)$value
    output$plot_rates_4 = renderDataTable(function_plot_rates_4(summary_data, input$rates_pathogen, input$rates_procedure, input$rates_agent, input$rates_year))
    
    plot_trans_1_input  = source('./scripts/plot_trans_1.R', local = TRUE)$value
    output$plot_trans_1 = renderPlotly(function_plot_trans_1(summary_data, input$trans_pathogen, input$trans_procedure, input$trans_year, input$trans_size))

    plot_trans_2_input  = source('./scripts/plot_trans_2.R', local = TRUE)$value
    output$plot_trans_2 = renderPlotly(function_plot_trans_2(summary_data, input$trans_isolate))
    
    plot_resis_1_input  = source('./scripts/plot_resis_1.R', local = TRUE)$value
    output$plot_resis_1 = renderPlotly(function_plot_resis_1(resistome_data))

    plot_resis_2_input  = source('./scripts/plot_resis_2.R', local = TRUE)$value
    output$plot_resis_2 = renderPlotly(function_plot_resis_2(resistome_data, input$genome))

    plot_resis_3_input  = source('./scripts/plot_resis_3.R', local = TRUE)$value
    output$plot_resis_3 = renderDataTable(function_plot_resis_3(resistome_data, input$genome))

    plot_timel_1_input  = source('./scripts/plot_timel_1.R', local = TRUE)$value
    output$plot_timel_1 = renderTimevis(function_plot_timel_1(summary_data, input$timel_patient))

    plot_timel_2_input  = source('./scripts/plot_timel_2.R', local = TRUE)$value
    output$plot_timel_2 = renderTimevis(function_plot_timel_2(summary_data, input$timel_isolate))
            
    plot_summ_1_input   = source('./scripts/plot_summ_1.R', local = TRUE)$value
    output$plot_summ_1  = renderPlotly(function_plot_summ_1(summary_data, input$summ_pathogen, input$summ_procedure))
    
    plot_summ_2_input   = source('./scripts/plot_summ_2.R', local = TRUE)$value
    output$plot_summ_2  = renderPlotly(function_plot_summ_2(summary_data, input$summ_pathogen, input$summ_procedure, input$summ_year))
    
  }
)
    
    
