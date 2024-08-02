# Ensure the necessary package is installed
if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
if (!require("GEOquery", quietly = TRUE))
  BiocManager::install("GEOquery")
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
library(GEOquery)
library(dplyr)
library(tidyr)
library(tidyverse)
library(shiny)
library(bslib)
library(visNetwork)
library(Seurat)

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
  list(
    navset_tab(
      id = "container",
      nav_panel("Workflow", card(
        fileInput("wl_file", "Choose a file"),
        selectInput("wl_file_options", 
                    "Choose an option", 
                    choices = list.files("workflows"),
                    selected = NULL)
      )),
      nav_panel("Step selected", uiOutput("dynamic_module"))
    ),
    card(
      card_header("Navigation Network"),
      visNetworkOutput("cwl_network")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  manager <- networkManager()
  manager$fromFile(file = "workflows/scrnaseq_seurat.json")
  
  # Create reactive shared steps inputs and outputs
  # shared_io <- reactive_wl_io(manager$getInputs(), manager$getOutputs(), manager$getSteps())
  shared_io <- reactiveValues()
  
  steps <- manager$getSteps() %>% select(id, out, module, label) %>% unnest_wider(c(out, module), names_sep=".") %>% drop_na()
  
  # Store the reactive outputs for each module
  module_outputs <- list()
  
  # Iterating through the steps
  for (i in 1:nrow(steps)) {
    s <- steps[i, ]
    shared_io <- do.call(s[["module.server"]], list(id = s[["module.id"]], shared_io = shared_io))
  }
  
  output$cwl_network <- renderVisNetwork({
    manager$visualizeNetwork() %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
  })
  
  observeEvent(input$cwl_network_selected, {
    nav_select("container", "Step selected")
    selected_node <- input$cwl_network_selected
    if (!is.null(selected_node) && selected_node %in% steps$id) {
      output$dynamic_module <- renderUI({
        card(
          card_header(steps[steps$id == selected_node, "label"]),
          do.call(steps[steps$id == selected_node, ][["module.ui"]], list(id = selected_node))
        )
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
