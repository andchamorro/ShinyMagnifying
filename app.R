library(shiny)
library(bslib)
library(dplyr)
library(DT)
library(visNetwork)
library(Seurat)
library(BiocManager)
library(GEOquery)

options(repos = BiocManager::repositories())
options(shiny.maxRequestSize = 30*1024^2)

# Define UI for application
ui <- page_navbar(
  title = "ShinyMagnifying",
  theme = bs_theme(preset = "minty", "primary" = "#0675DD"),
  collapsible = TRUE,
  nav_panel("Home",
            page_fluid(
              tags$head(
                tags$style("
                 .card_nav { resize: vertical; }
                 .card_plot { resize: both; }")
              ),
              list(
                navset_tab(
                  id = "container",
                  nav_panel("Workflow", card(
                    fileInput("wl_file", "Choose a file"),
                    selectInput("wl_file_options", 
                                "Choose an option", 
                                choices = list.files("workflows"),
                                selected = "scrnaseq_seurat.json")
                  )),
                  nav_panel("Step selected", uiOutput("dynamic_module"))
                ),
                card(
                  card_header("Navigation Network"),
                  height = "800px",
                  fill = TRUE,
                  class = 'card_nav',
                  card_body(
                    visNetworkOutput("cwl_network", width = "100%")
                  )
                )
              )
            )
  ),
  nav_panel(
    "Data export",
    card(
      card_header("Seurat Object"),
      DT::dataTableOutput("export")
    )
  ),
  nav_panel(
    "Help",
    card(
      card_header("About"),
      card_body(
        h2("About ShinyMagnifying"),
        p("This application is designed to help visualize and manage workflows.")
      )
    ),
    card(
      card_header("Documentation"),
      card_body(
        h2("Documentation"),
        p("Here you can provide documentation or links to documentation.")
      )
    )
  ),
  nav_item(
    input_dark_mode(id = "dark_mode", mode = "light")
  )
)

# Define server logic
server <- function(input, output, session) {
  cwl_manager <- list()
  observe({
    if (!is.null(input$wl_file)) {
      req(input$wl_file)
      cwl_manager <- read_cwl(file = input$wl_file$datapath)
    } else if (!is.null(input$wl_file_options)) {
      cwl_manager <- read_cwl(file = file.path("workflows", input$wl_file_options))
    }
    
    if (length(cwl_manager) > 0) {
      # Create reactive shared steps inputs and outputs
      shared_io <- reactiveValues()
      
      steps <- cwl_manager %>% parse_steps() %>% select(id, out, module, label) %>% unnest_wider(c("out", "module"), names_sep=".") %>% drop_na()
      
      # Store the reactive outputs for each module
      module_outputs <- list()
      
      # Iterating through the steps
      for (i in 1:nrow(steps)) {
        s <- steps[i, ]
        shared_io <- do.call(s[["module.server"]], list(id = s[["module.id"]], shared_io = shared_io))
      }
      
      output$cwl_network <- renderVisNetwork({
        cwl_manager %>% visualizeNetwork() %>%
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
  })
}

# Run the application
shinyApp(ui = ui, server = server)
