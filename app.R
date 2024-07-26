#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Ensure the necessary package is installed
if (!require(visNetwork)) {
  install.packages("visNetwork")
}
if (!require(Seurat)) {
  install.packages("Seurat")
}
if (!require(jsonlite)) {
  install.packages("tidycwl")
}

# Load necessary libraries
library(archive)
library(shiny)
library(bslib)
library(visNetwork)
library(Seurat)
# library(Scanpy)
# library(FastQC)
# library(STAR)
# library(CellRanger)
# library(Monocle)
# library(Slingshot)

options(shiny.maxRequestSize=30*1024^2)
sourceDir <- function(path, trace = TRUE, ...) {
  op <- options(); on.exit(options(op)) # to reset after each
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
    options(op)
  }
}
# Load the module file
sourceDir("R")

# Define UI for application
ui <- page_fluid(
  # Application title
  titlePanel("ShinyMagnifying"),
  wlUI(
    id = "main"
  )
)


# Define server logic
server <- function(input, output) {
  wlServer(
    id = "main",
    file = "workflows/scrnaseq_seurat.json"
  )
  # data_reactive <- dataServer("data")
  # seurat_obj <- qualitycontrolServer("qc", data_reactive)
}

# Run the application
shinyApp(ui = ui, server = server)
