dataInput <- function(id) {
  ns <- NS(id)
  tagList(
    # Input: Select file
    fileInput(ns("file"), "Upload your raw count matrix"),
    # Example files
    selectInput(ns("example_file"), "Or select an example file", choices = list.files("example")),
    # Input: Specify parameters
    numericInput(ns("min_genes"), "Minimum number of genes detected per cell", value = 200),
    numericInput(ns("max_genes"), "Maximum number of genes detected per cell", value = 2500),
    numericInput(
      ns("max_mito"),
      "Maximum proportion of mitochondrial genes",
      value = 0.05
    )
  )
}

dataServer <- function(id, shared_io){
  moduleServer(id, function(input, output, session) {
    # Load data
    data <- reactive({
      if (!is.null(input$file)) {
        # If file is uploaded
        req(input$file)
        dir_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", input$file$name)
        archive_extract(input$file$datapath, file.path(tempdir(), dir_name))
        data <- Read10X(file.path(tempdir(), dir_name))
      } else {
        # If example file is selected
        req(input$example_file)
        data <- Read10X(file.path("example", input$example_file))
      }
      return(data)
    })
    # Create seurat object
    reactive({
      seurat_obj <- CreateSeuratObject(counts = data())
      return(list("obj" = seurat_obj, "min_genes" = input$min_genes, "max_genes" = input$max_genes, "max_mito" = input$max_mito))
    })
  })
}

dataApp <- function() {
  # Define UI for application
  ui <- fluidPage(
      # Application title
      titlePanel("ShinyMagnifying"),
      # Sidebar layout with input and output definitions
      dataInput("seurat_data")
  )
  # Define server logic
  server <- function(input, output, session) {
    seurat_obj <- dataServer("seurat_data")
  }
  # Run the application
  shinyApp(ui = ui, server = server)
}

dataRun <- function() {
  
}