#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

options(shiny.maxRequestSize=30*1024^2)

# Load necessary libraries
library(archive)
library(shiny)
library(Seurat)
# library(Scanpy)
# library(FastQC)
# library(STAR)
# library(CellRanger)
# library(Monocle)
# library(Slingshot)

# Define UI for application
ui <- fluidPage(
  # Application title
  titlePanel("ShinyMagnifying"),
  
  # Sidebar layout with input and output definitions
  sidebarLayout(
    sidebarPanel(
      # Input: Select file
      fileInput("file", "Upload your raw count matrix"),
      # Input: Specify parameters
      numericInput("min_genes", "Minimum number of genes detected per cell", value = 200),
      numericInput("max_genes", "Maximum number of genes detected per cell", value = 2500),
      numericInput("max_mito", "Maximum proportion of mitochondrial genes", value = 0.05)
    ),
    
    # Output: Data summary and visualization
    mainPanel(
      plotOutput("quality_control"),
      plotOutput("pca_plot"),
      plotOutput("tsne_plot"),
      plotOutput("umap_plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Load data
  data <- reactive({
    # TODO
    req(input$file)
    dir_name <- sub(pattern = "(.*)\\..*$", replacement = "\\1", input$file$name)
    archive_extract(input$file$datapath, file.path(tempdir(), dir_name))
    data <- Read10X(file.path(tempdir(), dir_name))
    return(data)
  })
  
  # Preprocessing
  seurat_obj <- reactive({
    # Create Seurat object
    seurat_obj <- CreateSeuratObject(counts = data())
    seurat_obj <- PercentageFeatureSet(seurat_obj, "^MT-", col.name = "percent_mito")
    return(seurat_obj)
  })
  
  seurat_obj_qc <- reactive({
    # Quality control
    seurat_obj_qc <- subset(seurat_obj(), subset = nFeature_RNA > input$min_genes & nFeature_RNA < input$max_genes & percent_mito < input$max_mito)
    return(seurat_obj_qc)
  })
  
  # Normalization and scaling
  seurat_obj_norm <- reactive({
    # Normalize the data
    seurat_obj_norm <- NormalizeData(seurat_obj_qc())
    # Scale the data
    seurat_obj_norm <- ScaleData(seurat_obj_norm)
    return(seurat_obj_norm)
  })
  
  # PCA
  seurat_obj_pca <- reactive({
    # Run PCA
    seurat_obj_pca <- RunPCA(seurat_obj_norm())
    return(seurat_obj_pca)
  })
  
  # t-SNE
  seurat_obj_tsne <- reactive({
    # Run t-SNE
    seurat_obj_tsne <- RunTSNE(seurat_obj_pca())
    return(seurat_obj_tsne)
  })
  
  # UMAP
  seurat_obj_umap <- reactive({
    # Run UMAP
    seurat_obj_umap <- RunUMAP(seurat_obj_pca())
    return(seurat_obj_umap)
  })
  
  # Output: Data quality control
  output$quality_control <- renderPlot({
    VlnPlot(seurat_obj(), features = c("nFeature_RNA", "nCount_RNA", "percent_mito"), ncol = 3)
  })
  
  # Output: PCA plot
  output$pca_plot <- renderPlot({
    DimPlot(seurat_obj_pca(), reduction = "pca")
  })
  
  # Output: t-SNE plot
  output$tsne_plot <- renderPlot({
    DimPlot(seurat_obj_tsne(), reduction = "tsne")
  })
  
  # Output: UMAP plot
  output$umap_plot <- renderPlot({
    DimPlot(seurat_obj_umap(), reduction = "umap")
  })
}

# Run the application
shinyApp(ui = ui, server = server)