# UI Module for Normalization, Transformation, and PCA
normalizationUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("pca_plot"))
  )
}

# Server Module for Normalization, Transformation, and PCA
normalizationServer <- function(id, shared_io) {
  moduleServer(
    id,
    function(input, output, session) {
      seurat_obj_norm <- reactive({
        data_input <- shared_io()
        # Normalization
        data_input$obj <- NormalizeData(data_input$obj)
        # Transformation
        data_input$obj <- FindVariableFeatures(data_input$obj)
        data_input$obj <- ScaleData(data_input$obj)
        # PCA
        data_input$obj <- RunPCA(data_input$obj, npcs = 2)
        return(data_input)
      })
      
      # Output: PCA plot
      output$pca_plot <- renderPlot({
        DimPlot(seurat_obj_norm()$obj, reduction = "pca")
      })
      return(seurat_obj_norm)
    }
  )
}

# Shiny App for Normalization, Transformation, and PCA
normalizationApp <- function() {
  ui <- fluidPage(
    titlePanel("ShinyMagnifying - Normalization and PCA"),
    normalizationUI("seurat_data")
  )
  
  server <- function(input, output, session) {
    normalizationServer("seurat_data")
  }
  
  shinyApp(ui = ui, server = server)
}
