nextNode <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i + 1)), "next")
}
prevNode <- function(id, i) {
  actionButton(NS(id, paste0("go_", i, "_", i - 1)), "prev")
}

wlNode <- function(title, node, button_next = NULL, button_prev = NULL){
  tabPanel(
    title = title,
    fluidRow(
      column(6, node),
      column(6, verbatimTextOutput("env"))
    ),
    fluidRow(
      column(6, htmlOutput("network")),
      column(6, helpText("This is the help section."))
    ))
}

wlUI <- function(id) {
  ns <- NS(id)
  list(
    card(
      card_header("Step Node"),
      plotOutput(ns("step"))
    ),
    card(
      card_header("Navigation Network"),
      visNetworkOutput(ns("cwl_network"))
    )
  )
  # stopifnot(is.data.frame(manager$nodes))
  # n <- length(nodes)
  # wrapped <- vector("list", n)
  # for (i in seq_along(nodes)) {
  #   # First page only has next; last page only prev + done
  #   lhs <- if (i > 1) prevNode(id, i)
  #   rhs <- if (i < n) nextNode(id, i) else doneButton
  #   wrapped[[i]] <- cwlNode(paste0("node_", i), nodes[[i]], lhs, rhs)
  # }
  # 
  # # Create tabsetPanel
  # # https://github.com/rstudio/shiny/issues/2927
  # wrapped$id <- NS(id, "cwl")
  # wrapped$type <- "hidden"
  # do.call("tabsetPanel", wrapped)
}

wlServer <- function(id, file) {
  moduleServer(
    id,
    function(input, output, session) {
      manager <- networkManager()
      manager$fromFile(file = file)
      output$cwl_network <- renderVisNetwork({manager$visualizeNetwork()})
      # changeNode <- function(from, to) {
      #   observeEvent(input[[paste0("go_", from, "_", to)]], {
      #     updateTabsetPanel(session, "cwl", selected = paste0("node_", to))
      #   })
      # }
      # ids <- seq_len(n)
      # lapply(ids[-1], function(i) changeNode(i, i - 1))
      # lapply(ids[-n], function(i) changeNode(i, i + 1))
    }
  )
}

wlApp <- function(...) {
  nodes <- list(...)
  
  ui <- fluidPage(
    wlUI("wl", nodes)
  )
  server <- function(input, output, session) {
    wlServer("wl", length(nodes))
  }
  shinyApp(ui, server)
}