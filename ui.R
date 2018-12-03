library(timevis)
library(plotly)
library(shiny)

shinyUI(
  
  navbarPage(
    
    title = "PSP Resistance Map",

    # Trends Page
    tabPanel(
      "Trends",
      
      fluidPage(
        
        br(),
        br(),
        
        column(4, uiOutput("choose_rates_pathogen")),
        column(4, uiOutput("choose_rates_procedure")),
        column(4, uiOutput("choose_rates_func")),
        column(4, uiOutput("choose_rates_agent")),
        column(4, uiOutput("choose_rates_year")),
        
        
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        br(),
        hr(),
        
        h5("Resistant rates by year", style = "color:black"),
        h6("Select: pathogen and procedure", style = "color:gray"),
        h6("Description: Number of resistant rates for all years in PathogenDB", style = "color:gray"),
        bootstrapPage(plotlyOutput("plot_rates_1")),
        
        br(),
        br(),
        
        h5("Resistant rates by month", style = "color:black"),
        h6("Select: pathogen, procedure, agent and year", style = "color:gray"),
        h6("Description: Number of resistant rates for the selected year, by month", style = "color:gray"),
        bootstrapPage(plotlyOutput("plot_rates_2")),
        
        br(),
        br(),
        
        h5("Resistant rates by location", style = "color:black"),
        h6("Select: pathogen, procedure, year and agent", style = "color:gray"),
        h6("Description: Number of resistant rates for the selected and previous year, by location", style = "color:gray"),
        bootstrapPage(plotlyOutput("plot_rates_3")),
        
        br(),
        br(),
        
        h5("Table of isolates", style = "color:black"),
        h6("Select: pathogen, procedure, year and agent", style = "color:gray"),
        h6("Description: A table of all isolates matching your selection criteria", style = "color:gray"),
        mainPanel(dataTableOutput("plot_rates_4"))
        
      )
    ),
    
    # Persistence Page
    tabPanel(
      "Persistence",
      
      fluidPage(
        
        br(),
        br(),
        
        column(3, uiOutput("choose_persi_pathogen")),
        column(3, uiOutput("choose_persi_procedure")),
        column(3, uiOutput("choose_persi_agent")),
        column(3, uiOutput("choose_persi_patient")),

        br(),
        br(),
        br(),
        br(),
        hr(),
        
        h5("Persistence & Heteroresistance", style = "color:black"),
        h6("Select: pathogen and procedure", style = "color:gray"),
        h6("Proportion of cases that are single, persistent or heteroresistant cases", style = "color:gray"),
        bootstrapPage(plotlyOutput("plot_persi_1")),
        
        br(),
        br(),
        
        h5("Mean MIC", style = "color:black"),
        h6("Select: pathogen and procedure", style = "color:gray"),
        h6("Average MIC for persistent and single sample infections", style = "color:gray"),
        bootstrapPage(plotlyOutput("plot_persi_2")),
        
        br(),
        br(),
        
        h5("Heteroresistance", style = "color:black"),
        h6("Select: pathogen and procedure", style = "color:gray"),
        h6("Left: Number of cases with a change in susceptibility phenotype for a given agent. Right: Percent of heteroresistant cases with a given time between isolates", style = "color:gray"),
        
        fluidRow(
          splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("plot_persi_3"), plotlyOutput("plot_persi_4")
                      
          )
        ),
        
        br(),
        br(),
        
        h5("MIC Trends", style = "color:black"),
        h6("Select: pathogen, procedure, agent and patient", style = "color:gray"),
        h6("MIC for all isolates of a select patient and agent", style = "color:gray"),
        bootstrapPage(plotlyOutput("plot_persi_5")),
        
        br(),
        br()
        
      )
    ),
    
    # Transmission Page
    tabPanel(
      "Transmissions",
      
      fluidPage(
        
        br(),
        br(),
        
        column(3, uiOutput("choose_trans_pathogen")),
        column(3, uiOutput("choose_trans_procedure")),
        column(3, uiOutput("choose_trans_year")),
        column(3, uiOutput("choose_trans_size")),
        
        br(),
        br(),
        br(),
        br(),
        hr(),
        
        h5("Transmission", style = "color:black"),
        h6("Select: pathogen, procedure, year and cluster size", style = "color:gray"),
        h6("Clusters of shared MIC profiles and locations", style = "color:gray"),
        bootstrapPage(plotlyOutput("plot_trans_1")),
        
        br(),
        br(),
        
        column(3, uiOutput("choose_trans_isolate")),
        
        br(),
        br(),
        br(),
        br(),
        hr(),
        
        h5("Cluster by isolate", style = "color:black"),
        h6("Select: Isolate ID. Pathogen, procedure, year and cluster size selections do not apply.", style = "color:gray"),
        h6("All isolates of the same procedure that share the same MIC profile.", style = "color:gray"),
        bootstrapPage(plotlyOutput("plot_trans_2")),
        
        br(),
        br()
        
      )
    ),
    
    # Timeline Page
    tabPanel(
      "Timelines",
      
      fluidPage(
        
        br(),
        br(),
        
        column(3, uiOutput("choose_timel_patient")),
        
        br(),
        br(),
        br(),
        br(),
        hr(),
        
        h5("Patient Timeline", style = "color:black"),
        h6("Select: patient", style = "color:gray"),
        h6("Timeline of all samples collected from the selected patient", style = "color:gray"),
        bootstrapPage(timevisOutput("plot_timel_1")),
        
        br(),
        br(),
        
        column(3, uiOutput("choose_timel_isolate")),
        
        br(),
        br(),
        br(),
        br(),
        hr(),
        
        h5("Transmission Timeline", style = "color:black"),
        h6("Select: isolate", style = "color:gray"),
        h6("Timeline of all samples with matching susceptibility profile to selected isolate", style = "color:gray"),
        bootstrapPage(timevisOutput("plot_timel_2")),
        
        br(),
        br()
        
      )
    ),
    
    # Resistome Page
    tabPanel(
      "Resistome",
      
      fluidPage(
        
        br(),
        br(),
        
        column(3, uiOutput("choose_species")),
        
        br(),
        br(),
        br(),
        br(),
        hr(),
        
        h5("Frequency", style = "color:black"),
        h6("Frequency of resistance genes and variants by genomic feature", style = "color:gray"),
        bootstrapPage(plotlyOutput("plot_resis_1")),
        
        br(),
        br(),
        
        wellPanel(uiOutput("choose_genome")),
        
        hr(),
        
        h5("Resistome profiles", style = "color:black"),
        h6("Select: Two or more genomes", style = "color:gray"),
        h6("Clustered resistome profiles", style = "color:gray"),
        bootstrapPage(plotlyOutput("plot_resis_2")),
        
        br(),
        br(),
        
        h5("By genome", style = "color:black"),
        h6("Select: genome", style = "color:gray"),
        h6("Resistance determinents for selected genome(s)", style = "color:gray"),
        dataTableOutput("plot_resis_3")
        
      )
    ),
    
    # About Page
    tabPanel(
      "About",
      
      fluidPage(
        
        br(),
        br(),
        
        column(12,
               h3("PSP Resistance Map", align = "center"),
               
               hr(),
               
               fluidRow(
                 
                 column(6,
                        h5("Goal:"),
                        h5("The PSP Resistance Map provides a real-time view at the changing prevalence of antibiotic resistant pathogens at the
                          Mount Sinai Hospital with the goal of aiding future solutions that address the growing threat of drug-resistant
                          pathogens in the hospital.", style = "color:gray"),
                        h5("The tool provides broad analysis on both clinical and genomic data sourced from PathogenDB. Analysis of critical data
                          focuses on key hospital pathogens: Acinetobacter baumannii, Enterobacter cloacae, Enterococcus faecalis,
                          Enterococcus faecium, Klebsiella pneumoniae, Pseudomonas aeruginosa, and Staphylococcus aureus. Genomic data focuses on
                          Staphylococcus aureus and Clostridium Difficile.", style = "color:gray"),
                        h5("How to use:"),
                        h5("Select a tab for the type of analysis. On each tab, you can select criteria to customize the analysis: Pathogen
                           (species of pathogen), Procedure (source of sample), Agent (antibiotic), and Year. Above each plot, there are directions
                           on what criteria apply to the plot, as well as a description.", style = "color:gray"),
                        h5("Sources:"),
                        h5(" - CDDEP Resistance Map: https://resistancemap.cddep.org", style = "color:gray"),
                        h5(" - Chacko KI, Sullivan MJ, et al. AAC (2018)", style = "color:gray"),
                        h5(" - Pak TR, Altman DR, et al. AAC (2015)", style = "color:gray"),
                        h5(" - Altman DR, Sullivan MJ, et al. In preparation (2018)", style = "color:gray")
                        
                 ),
                 
                 column(6,
                        h5("Trends:"),
                        h5("Changing number of cases and percent of resistant cases over time. It considers the first isolate by species
                          for each patient.", style = "color:gray"),
                        h5("Transmissions:"),
                        h5("Potential transmissions represent cases that have identical susceptibility profiles, including MICs, and were collected in the
                          same location. No processing of MIC results were performed (eg. >=32 vs. >32 are considered different). Select a minimum cluster
                          size to find larger or smaller transmissions. You can also find clusters related to an isolate of interest.", style = "color:gray"),
                        h5("Heteroresistance:"),
                        h5("Patients with multiple samples collected that either have consistent susceptibility profiles (persistent) or changing susceptibility
                           profiles (heteroresistent) over time.", style = "color:gray"),
                        h5("Resistome:"),
                        h5("Antibiotic resistance determinants annotated in sequenced genomes. Select multiple isolates to see how their resistome profiles
                          cluster with one another. All results are shown in the table at the bottom of the page.", style = "color:gray")
                        
                 )
               )
        ),

        br(),
        br(),
        br(),
        br(),
        
        fluidPage(
          
          column(12,
          
          br(),
          br(),
          
          column(4, uiOutput("choose_summ_pathogen")),
          column(4, uiOutput("choose_summ_procedure")),
          column(4, uiOutput("choose_summ_year")),
          
          br(),
          br(),
          br(),
          br(),
          hr(),
          
          h5("Number of isolates by year", style = "color:black"),
          h6("Select: pathogen and procedure", style = "color:gray"),
          h6("Number of unique isolates collected per year", style = "color:gray"),
          bootstrapPage(plotlyOutput("plot_summ_1")),
          
          br(),
          br(),
          
          h5("Number of isolates by location", style = "color:black"),
          h6("Select: pathogen and procedure", style = "color:gray"),
          h6("Number of unique isolates collected per location", style = "color:gray"),
          bootstrapPage(plotlyOutput("plot_summ_2"))
          
          )
        ),
        
        # Remove error messages
        tags$style(type="text/css", ".shiny-output-error { visibility: hidden; }", ".shiny-output-error:before { visibility: hidden; }")
        
      )
    )
  )
)
          
        # 
        # br(),
        # br(),
        # 
        # h5("MDR prevalence and severeity", style = "color:gray"),
        # bootstrapPage(plotlyOutput("plot_summ_2")),
        # 
        # br(),
        # br(),
        # 
        # h5("Antibiotic utility", style = "color:gray"),
        # bootstrapPage(plotlyOutput("plot_summ_3")),
        # 
        # br(),
        # br(),
        # 
        # h5("Location hotspots", style = "color:gray"),
        # bootstrapPage(plotlyOutput("plot_summ_4")),
        
        