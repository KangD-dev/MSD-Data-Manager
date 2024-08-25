# Set environment
library(shiny)
library(bslib)
library(plotly)



# Define UI
ui <- page_navbar(
  title = "MSD Data Manager",
  
  # Sidebar configuration
  sidebar = sidebar(
    card(
      fileInput("result", 
                label = "Upload Result File(s):",
                multiple = TRUE,
                accept = ".csv"),
      helpText("Upload one or more CSV files containing the results.")
    ),
    card(
      fileInput("standard",
                label = "Upload Standard File(s):",
                multiple = TRUE,
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
  
  # Navigation panels
  nav_panel("QC",
            uiOutput("qc_ui")  # Dynamic UI for dot plots
  ),
  
  nav_panel("Result",
            # Add Result related UI elements here
  ),
  
  nav_panel("Analysis",
            # Add Analysis related UI elements here
  )
)



# Define Server
server <- function(input, output, session) {
  
  # --- QC Page ---
  # Reactive to handle standard file uploads and generate plots
  observe({
    req(input$standard)
    
    # Create dynamic UI for each standard file
    output$qc_ui <- renderUI({
      plot_output_list <- lapply(1:length(input$standard$name), function(i) {
        plotlyOutput(outputId = paste0("dotPlot-", i))
      })
      do.call(tagList, plot_output_list)
    })
    
    # Generate dot plots for each file
    lapply(1:length(input$standard$name), function(i) {
      output[[paste0("dotPlot-", i)]] <- renderPlotly({
        # Read the CSV file
        data <- read.csv(input$standard$datapath[i])
        
        # Create a dot plot (customize based on your data structure)
        ggplot(data)
        # ggplot(data, aes(x = factor(1), y = <your_y_variable>)) + 
        #   geom_dotplot(binaxis = 'y', stackdir = 'center') +
        #   labs(title = paste("Dot Plot for", input$standard$name[i]),
        #        x = NULL, y = "<Your Y Axis Label>") +
        #   theme_minimal()
      })
    })
  })
}

  


# Run the application 
shinyApp(ui = ui, server = server)
