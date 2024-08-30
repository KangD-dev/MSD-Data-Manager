# Set environment
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(readxl)
library(janitor)
library(tidyverse)

# Support Function
source("src/support_function.R")

# Define UI
ui <- page_navbar(
  title = h1("MSD Data Manager"),
  
  # Sidebar configuration
  nav_panel("Upload Files",
            sidebarLayout(
              sidebarPanel(
                card(
                  fileInput("result", 
                            label = "Upload Result File(s):",
                            multiple = FALSE,
                            accept = ".csv"),
                  helpText("Upload one or more CSV files containing the results.")
                ),
                
                card(
                  fileInput("standard",
                            label = "Upload Standard File(s):",
                            multiple = FALSE,
                            accept = ".csv"),
                  helpText("Upload one or more CSV files containing the standard data.")
                ),
                
                card(
                  fileInput("spl_man",
                            label = "Upload Sample Manifest File:",
                            multiple = FALSE,
                            accept = ".xlsx"),
                  helpText("Upload the Sample Manifest file in XLSX format.")
                )
              ),
              mainPanel(
                uiOutput("result_ui")  # Dynamic UI for results
              )
            )
  ),
  
  # Navigation panels
  nav_panel("Quality Control (QC)",
            uiOutput("qc_ui")  # Dynamic UI for QC
  ),
  
  nav_panel("Data Analysis",
            uiOutput("analysis_ui") # Dynamic UI for analysis
  ),
  
  theme = bs_theme(preset = "materia")
)

# Define Server
server <- function(input, output, session) {
  
  
  observe({
    req(input$result, input$standard, input$spl_man)
    
    # preprocess MSD file
    msd <- preprocessMSD(dataFile = input$result$datapath, standardFile = input$standard$datapath)
    
    # add sample manifest
    spl <- read_excel(input$spl_man$datapath, sheet = "Sample Manifest")
    metadata <- spl %>% left_join(msd, by = c("SAMPLE" = "sample"))
    
    # --- Result UI ---
    # Create dynamic result UI
    output$result_ui <- renderUI({
      DT::DTOutput("result_tbl")
    })

    output$result_tbl <- renderDT({
      metadata
    })
    
    # --- QC UI ---
    output$qc_ui <- renderUI({
      plotlyOutput("plot_cv")
    })
    
    output$plot_cv <- renderPlotly({
      plotCV(metadata)
    })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
