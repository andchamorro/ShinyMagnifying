qualitycontrolUI <- function(id) {
  ns <- NS(id)
  # Output: Data summary and visualization
  tagList(
    plotOutput(ns("quality_control"))
  )
}

qualitycontrolServer <- function(id, data_reac) {
  moduleServer(
    id,
    function(input, output, session) {
      seurat_obj_qc <- reactive({
        data_input <- data_reac()
        # Quality control
        seurat_obj_qc <- subset(data_input$obj, subset = nFeature_RNA > data_input$min_genes & nFeature_RNA < data_input$max_genes & percent_mito < data_input$max_mito)
        return(seurat_obj_qc)
      })
      
      # Output: Data quality control
      output$quality_control <- renderPlot({
        VlnPlot(seurat_obj_qc(), features = c("nFeature_RNA", "nCount_RNA", "percent_mito"), ncol = 3)
      })
    })
}

qualitycontrolApp <- function() {
  # Define UI for application
  ui <- fluidPage(
    # Application title
    titlePanel("ShinyMagnifying"),
    qualitycontrolUI("seurat_data")
  )
  # Define server logic
  server <- function(input, output, session) {
    qualitycontrolServer("seurat_data")
  }
  # Run the application
  shinyApp(ui = ui, server = server)
}