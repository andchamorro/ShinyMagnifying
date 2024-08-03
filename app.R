# Load necessary libraries
library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(visNetwork)
library(Seurat)
library(BiocManager)
options(repos = BiocManager::repositories())
library(GEOquery)

# Load the module file
# sourceDir("R")

options(shiny.maxRequestSize=30*1024^2)
# Define UI for application
ui <- page_fluid(
  tags$head(
    tags$style("
      .card_nav {
        resize: vertical;
      }
      .card_plot {
        resize: both;
      }
    ")
  ),
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
      height = "30%",
      fill = FALSE,
      class = 'card_nav',
      card_body(
        visNetworkOutput("cwl_network")
      )
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
  
  steps <- manager$getSteps() %>% select(id, out, module, label) %>% unnest_wider(c("out", "module"), names_sep=".") %>% drop_na()
  
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
          height = "30%",
          fill = FALSE,
          class = 'card_plot',
          card_header(steps[steps$id == selected_node, "label"]),
          card_body(
            do.call(steps[steps$id == selected_node, ][["module.ui"]], list(id = selected_node))
          )
        )
      })
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
