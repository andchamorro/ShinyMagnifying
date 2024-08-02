qualitycontrolUI <- function(id) {
  ns <- NS(id)
  # Output: Data summary and visualization
  sidebarLayout(
    sidebarPanel(
      # Input: Specify parameters
      numericInput(ns("min_genes"), "Minimum number of genes detected per cell", value = 200),
      numericInput(ns("max_genes"), "Maximum number of genes detected per cell", value = 2500),
      numericInput(
        ns("max_mito"),
        "Maximum proportion of mitochondrial genes",
        value = 5
      )
    ),
    mainPanel(
      plotOutput(ns("quality_control"))
    )
  )
}

qualitycontrolServer <- function(id, shared_io) {
  moduleServer(
    id,
    function(input, output, session) {
      seurat_obj_qc <- reactive({
        data_input <- shared_io()
        # Quality control
        data_input$obj <- PercentageFeatureSet(data_input$obj, "^MT-", col.name = "percent_mito")
        seurat_obj_qc <- subset(data_input$obj, subset = nFeature_RNA > input$min_genes & nFeature_RNA < input$max_genes & percent_mito < input$max_mito)
        return(seurat_obj_qc)
      })
      # Output: Data quality control
      output$quality_control <- renderPlot({
        VlnPlot(seurat_obj_qc(), features = c("nFeature_RNA", "nCount_RNA", "percent_mito"), ncol = 3)
      })
      reactive({
        return(list("obj" = seurat_obj_qc(), "min_genes" = input$min_genes, "max_genes" = input$max_genes, "max_mito" = input$max_mito))
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