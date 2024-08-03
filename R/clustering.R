# UI Module for Clustering and Biomarker Identification
clusteringUI <- function(id) {
  ns <- NS(id)
  # tagList(
  #   plotOutput(ns("umap_plot")),
  #   DTOutput(ns("biomarkers_table"))
  # )
  fluidRow(
    column(6, dataTableOutput(ns("biomarkers_table"))),
    column(6, plotOutput(ns("umap_plot"), height = 500))
  )
}

# Server Module for Clustering and Biomarker Identification
clusteringServer <- function(id, shared_io) {
  moduleServer(
    id,
    function(input, output, session) {
      seurat_obj_cluster <- reactive({
        data_input <- shared_io()
        # Clustering
        data_input$obj <- FindNeighbors(data_input$obj, dims = 1:10)
        data_input$obj <- FindClusters(data_input$obj, resolution = 0.5)
        # UMAP
        data_input$obj <- RunUMAP(data_input$obj, dims = 1:10)
        return(data_input)
      })
      
      biomarkers <- reactive({
        data_input <- seurat_obj_cluster()
        # Biomarker Identification
        data_input$biomarkers <- FindAllMarkers(data_input$obj, only.pos = TRUE, min.pct = 0.25, logfc.threshold = 0.25)
        return(data_input)
      })
      
      # Output: UMAP plot
      output$umap_plot <- renderPlot({
        DimPlot(biomarkers()$obj, reduction = "umap", label = TRUE)
      })
      
      # Output: Biomarkers table
      output$biomarkers_table <- renderDT({
        biomarkers()$biomarkers
      })
      
      return(biomarkers)
    }
  )
}

# Shiny App for Clustering and Biomarker Identification
clusteringApp <- function() {
  ui <- fluidPage(
    titlePanel("ShinyMagnifying - Clustering and Biomarker Identification"),
    clusteringUI("seurat_data")
  )
  
  server <- function(input, output, session) {
    clusteringServer("seurat_data")
  }
  
  shinyApp(ui = ui, server = server)
}