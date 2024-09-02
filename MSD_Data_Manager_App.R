# Set environment
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(readxl)
library(janitor)
library(tidyverse)
library(stringr)



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
                ),
                
                # Add download button
                downloadButton("download_metadata", "Download Metadata")
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
  
  nav_panel("Data Analysis (Under Development)",
            uiOutput("analysis_ui") # Dynamic UI for analysis
  ),
  
  theme = bs_theme(preset = "materia")
)



# Define Server
server <- function(input, output, session) {
  
  # Reactive expression to preprocess the data and merge metadata
  metadata_reactive <- reactive({
    req(input$result, input$standard, input$spl_man)
    
    # preprocess MSD file
    msd <- preprocessMSD(dataFile = input$result$datapath, standardFile = input$standard$datapath)
    
    # add sample manifest
    spl <- read_excel(input$spl_man$datapath, sheet = "Sample Manifest")
    spl %>% left_join(msd, by = c("SAMPLE" = "sample"))
  })
  
  observe({
    req(metadata_reactive())
    
    metadata <- metadata_reactive()
    
    # --- Result UI ---
    output$result_ui <- renderUI({
      DT::DTOutput("result_tbl")
    })
    
    output$result_tbl <- renderDT({
      metadata
    })
    
    # --- QC UI ---
    output$qc_ui <- renderUI({
      # Dynamically create plotlyOutput elements for each plot
      plot_output_list <- lapply(1:length(unique(metadata$assay)), function(i) {
        plotlyOutput(paste0("plot_cv_", i))
      })
      # Combine the list into a tagList for sequential display
      do.call(tagList, plot_output_list)
    })
    
    # Render each plot
    lapply(1:length(unique(metadata$assay)), function(i) {
      output[[paste0("plot_cv_", i)]] <- renderPlotly({
        assay_data <- metadata %>% filter(assay == unique(metadata$assay)[i])
        plotQC(assay_data)
      })
    })
  })
  
  
  # --- Download Handler ---
  output$download_metadata <- downloadHandler(
    filename = function() {
      paste0(str_remove(input$result$name, ".csv"), "_metadata_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(metadata_reactive(), file, row.names = FALSE)
    }
  )
}



# Run the application 
shinyApp(ui = ui, server = server)
