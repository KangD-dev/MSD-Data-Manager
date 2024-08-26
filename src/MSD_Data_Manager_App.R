# Set environment
library(shiny)
library(bslib)
library(plotly)
library(readxl)
library(janitor)



# Define UI
ui <- page_navbar(
  title = h1("MSD Data Manager"),
  
  # Sidebar configuration
  sidebar = sidebar(
    card(
      card_title(h3("Formatting & QC")),
      fileInput("result", 
                label = "Upload Result File(s):",
                multiple = TRUE,
                accept = ".csv"),
      helpText("Upload one or more CSV files containing the results."),
      
      fileInput("standard",
                label = "Upload Standard File(s):",
                multiple = TRUE,
                accept = ".csv"),
      helpText("Upload one or more CSV files containing the standard data.")
    ),
    card(
      card_title(h3("Data Analysis")),
      fileInput("spl_man",
                label = "Upload Sample Manifest File:",
                multiple = FALSE,
                accept = ".xlsx"),
      helpText("Upload the Sample Manifest file in XLSX format."),
      
      fileInput("formatted_result",
                label = "Upload Formatted Result File:",
                multiple = FALSE,
                accept = ".xlsx"),
      helpText("Upload the formatted result file in XLSX format for further analysis.")
    )
  ),
  
  # Navigation panels
  nav_panel("Result Overview",
            uiOutput("result_ui")  # Dynamic UI for result overview
  ),
  
  nav_panel("Quality Control (QC)",
            uiOutput("qc_ui")  # Dynamic UI for QC
  ),
  
  nav_panel("Data Analysis",
            uiOutput("analysis_ui") # Dynamic UI for analysis
  ),
  
  theme = bs_theme(preset = "litera")
)



# Define Server
server <- function(input, output, session) {

  # --- Result Overview Page ---
  observe({
    req(input$result, input$standard)
    
    
  })
  
  
  # # Reactive to handle standard file uploads and generate plots
  # observe({
  #   req(input$standard)
  #   
  #   # Create dynamic UI for each standard file
  #   output$qc_ui <- renderUI({
  #     plot_output_list <- lapply(1:length(input$standard$name), function(i) {
  #       plotlyOutput(outputId = paste0("dotPlot-", i))
  #     })
  #     do.call(tagList, plot_output_list)
  #   })
  #   
  #   # Generate dot plots for each file
  #   lapply(1:length(input$standard$name), function(i) {
  #     output[[paste0("dotPlot-", i)]] <- renderPlotly({
  #       # Read the CSV file
  #       data <- read.csv(input$standard$datapath[i])
  #       
  #       # Create a dot plot (customize based on your data structure)
  #       ggplot(data)
  #       # ggplot(data, aes(x = factor(1), y = <your_y_variable>)) + 
  #       #   geom_dotplot(binaxis = 'y', stackdir = 'center') +
  #       #   labs(title = paste("Dot Plot for", input$standard$name[i]),
  #       #        x = NULL, y = "<Your Y Axis Label>") +
  #       #   theme_minimal()
  #     })
  #   })
  # })
}

  


# Run the application 
shinyApp(ui = ui, server = server)
