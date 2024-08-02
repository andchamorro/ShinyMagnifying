rename_files <- function(directory_path) {
  # Obtain a list of all files in the directory
  files <- list.files(directory_path, full.names = TRUE)
  
  # Define the regular expression pattern
  pattern <- ".*(barcodes|features|genes|matrix)\\.(tsv|mtx)\\.gz"
  
  # Iterate over each file
  for (file_path in files) {
    # Extract the file name from the path
    file_name <- basename(file_path)
    
    # Check if the file name matches the pattern
    if (grepl(pattern, file_name)) {
      # Construct the new file name
      new_name <- sub(".*(barcodes|features|genes|matrix)\\.(tsv|mtx)\\.gz", "\\1.\\2.gz", file_name)
      # If the new name is "genes.tsv.gz", change it to "features.tsv.gz"
      new_name <- if (new_name == "genes.tsv.gz") "features.tsv.gz" else new_name
      # Construct the new file path
      new_file_path <- file.path(dirname(file_path), new_name)
      # Rename the file
      file.rename(file_path, new_file_path)
      cat("Renamed", file_name, "to", new_name, "\n")
    }
  }
}

dataInput <- function(id) {
  ns <- NS(id)
  tagList(
    # Input: Project name
    textInput(ns("pname"), "Project name", "pbmc3k"),
    # Input: Select file
    fileInput(ns("file"), "Upload your raw count matrix"),
    # Url: 
    textInput(ns("geo"), "GEO accession number such as GSM3535276", ""),
    # Example files
    selectInput(ns("example_file"), "Or select an example file",
                choices = list.files("example"),
                selected = "pbmc3k_filtered_gene_bc_matrices")
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
        # Auto-detect file type and read data accordingly
        if (grepl("\\.tar.gz$", input$file$name)) {
          archive_extract(input$file$datapath, file.path(tempdir(), dir_name))
          data <- Read10X(file.path(tempdir(), dir_name))
        } else if (grepl("\\.h5$", input$file$name)) {
          data <- Read10X_h5(file.path(tempdir(), dir_name)) 
        } else if (grepl("\\.tiff$|\\.png$|\\.jpg$", input$file$name)) {
          data <- Read10X_Image(image.dir = file.path(tempdir(), dir_name))
        } else {
          # Unsupported file
        }
      } else if (nzchar(input$geo)) {
        query <- getGEOSuppFiles(input$geo, baseDir=tempdir(), fetch_files = TRUE, filter_regex = "matrix\\.mtx|barcodes\\.tsv|genes\\.tsv")
        rename_files(file.path(tempdir(), input$geo))
        data <- Read10X(file.path(tempdir(), input$geo))
      } else {
        # If example file is selected
        req(input$example_file)
        data <- Read10X(file.path("example", input$example_file))
      }
      return(data)
    })
    
    # Create seurat object
    reactive({
      seurat_obj <- CreateSeuratObject(counts = data(), project = input$pname)
      return(list("obj" = seurat_obj))
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
  dataApp()
}