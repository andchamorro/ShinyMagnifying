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

reactive_wl_io <- function(wl_inputs, wl_outputs, wl_steps){
  inputs <- rep(list(NULL), length(wl_inputs$id))
  names(inputs) <- wl_inputs$id
  
  outputs <- rep(list(NULL), length(wl_outputs$id))
  names(outputs) <- wl_outputs$id
  
  steps <- rep(list(NULL), length(wl_steps$id))
  names(steps) <- wl_steps$id
  
  do.call(reactiveValues, c(inputs, outputs, steps))
}

wlUI <- function(id) {
  ns <- NS(id)
  list(
    navset_tab(
      id = ns("container"),
      nav_panel("Workflow", card(
        fileInput(ns("wl_file"), "Choose a file"),
        selectInput(ns("wl_file_options"), 
                    "Choose an option", 
                    choices = list.files("workflows"),
                    selected = NULL)
        # uiOutput(ns("active_node"))
        ))
    ),
    card(
      card_header("Navigation Network"),
      visNetworkOutput(ns("cwl_network"))
    )
  )
  # list(
  #   card(
  #     card_header("Step Node"),
  #     uiOutput(ns("active_node"))
  #   ),
  #   card(
  #     card_header("Navigation Network"),
  #     visNetworkOutput(ns("cwl_network"))
  #   )
  # )
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

wlServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      reac <- NULL
      observe({
        if(!is.null(input$wl_file) || !is.null(input$wl_file_options)){
          manager <- networkManager()
          manager$fromFile(file = if(!is.null(input$wl_file)) input$wl_file$datapath else paste("workflows",input$wl_file_options, sep="/"))
          # Create reactive shared steps inputs and outputs
          shared_io <- reactive_wl_io(manager$getInputs(), manager$getOutputs(), manager$getSteps())
          # Create navigations tabs
          steps <- manager$getSteps() %>% select(id, out, module) %>% unnest_wider(c(out, module), names_sep=".") %>% drop_na()
          # modules <- manager$getSteps() %>% select(id, module) %>% unnest_wider(module, names_sep=".") %>% drop_na()
          # apply(steps, 1, function(s) {
          #   nav_insert(
          #     id = "container",
          #     nav = nav_panel(
          #       paste("nav_panel", s[["module.id"]], sep="_"),
          #       card(do.call(s[["module.ui"]], list(id = s[["module.id"]])))
          #     )
          #   )
          #   # reac <- do.call(s[["module.server"]], list(id = s[["module.id"]], shared_io = reac))
          # })
          # steps <- manager$getSteps() %>% select(id, out, module) %>% unnest_wider(c(out, module), names_sep=".") %>% drop_na()
          for (i in 1:nrow(steps)) {
            s <- steps[i, ]
            nav_insert(
              id = "container",
              nav = nav_panel(
                s[["module.id"]],
                card(do.call(s[["module.ui"]], list(id = s[["module.id"]])))
              )
            )
          }
          for (i in 1:nrow(steps)) {
            s <- steps[i, ]
            reac <- do.call(s[["module.server"]], list(id = s[["module.id"]], shared_io = reac))
          }
          reac()
          
          output$cwl_network <- renderVisNetwork({
            manager$visualizeNetwork() %>%
              visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE)
          })
        }})
      
      # nav_insert(
      #   id = "container",
      #   nav = nav_panel("Navigation Network", card(uiOutput(ns("next_node"))))
      # )
      # active_step <- reactive({
      #   step <- manager$getStep(input$cwl_network_selected)
      #   if (nrow(step) != 0) {
      #     shared_io$out_seurat_object <- do.call(step$module$server,
      #             list(id = step$module$id,
      #                  step = step, reactive_io = shared_io))
      #   }
      #   return(step)
      # })
      # observe({
      #   tryCatch({
      #     print(shared_io[[active_step()$id]])
      #     shared_io[[active_step()$id]][[active_step()$out$id]] <- do.call(
      #       active_step()$module$server, 
      #       list(id = active_step()$module$id,
      #            step = active_step(), reactive_io = shared_io)
      #     )
      #   }, error = function(e) {
      #     print(e)
      #   })
      # })
      # output$next_node <- renderUI({renderText("Next Node")})
      # output$active_node <- renderUI({
      #   tryCatch({
      #     do.call(active_step()$module$ui, list(id = active_step()$module$id))
      #   }, error = function(e) {
      #     renderText({
      #       "No step selected"
      #     })
      #   })
      # })
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