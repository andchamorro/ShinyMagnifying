# Usage
# manager <- networkManager()
# manager$addNode(1, "Node 1")
# manager$addNode(2, "Node 2")
# manager$visualizeNetwork()

# Define the network manager module
networkManager <- function() {
  
  # Initialize an empty flow
  flow <- list()
  # Initialize an empty dataframe to store the nodes and edges
  nodes <- data.frame()
  edges <- data.frame()
  
  # Function to add a node to the network
  addNode <- function(id, label, group, ui, server) {
    node <- data.frame(id = id, label = label, group = group, ui = ui, server = server, stringsAsFactors = FALSE)
    nodes <<- rbind(nodes, node)
  }
  
  # Function to remove a node from the network
  removeNode <- function(id) {
    nodes <<- nodes[!(nodes$id == id), ]
  }
  
  # Function to add a dependencies edges
  addEdge <- function(from, to, port_from, port_to, type) {
    edge <- data.frame(from = from, to = to, port_from = port_from, port_to = port_to, type = type, stringsAsFactors = FALSE)
    edges <<- rbind(edges, edge)
  }
  # Get step by id
  getStep <- function(id){
    tryCatch({
      flow$steps[flow$steps$id == id, ]
    }, error = function(e) {
      NULL
    })
  }
  # Function to read from file
  fromFile <- function(file, format = c("json", "yaml")){
    if (length(flow) > 0) flow <<- list()
    flow <<- read_cwl(file = file, format = "json")
    nodes <<- get_nodes(
      flow %>% parse_inputs(),
      flow %>% parse_outputs(),
      flow %>% parse_steps()
    )
    edges <<- get_edges(
      flow %>% parse_outputs(),
      flow %>% parse_steps()
    )
  }
  
  # Function to visualize the network
  visualizeNetwork <- function(hierarchical = TRUE, direction = "LR", separation = 300,
                               palette = c("#C3C3C3", "#FF8F00", "#00AAA8"),
                               width = "300%", height = "%100") {
    visNetwork(nodes = nodes, edges = edges, width = width) %>%
      visNodes(borderWidth = 2) %>%
      visEdges(
        arrows = list(to = list(enabled = TRUE, scaleFactor = 0.5)),
        smooth = list(type = "cubicBezier", roundness = 0.6)
      ) %>%
      visGroups(groupname = "input", color = palette[1], shadow = list(enabled = TRUE)) %>%
      visGroups(groupname = "output", color = palette[2], shadow = list(enabled = TRUE)) %>%
      visGroups(groupname = "step", color = palette[3], shadow = list(enabled = TRUE)) %>%
      visHierarchicalLayout(
        enabled = hierarchical,
        direction = direction, levelSeparation = separation,
        sortMethod = "directed",
      )
  }
  
  # Return the module functions
  list(
    label = flow %>% get_label(),
    getInputs = function() flow %>% parse_inputs,
    getOutputs = function() flow %>% parse_outputs,
    getSteps = function() flow %>% parse_steps,
    getStep = getStep,
    fromFile = fromFile,
    visualizeNetwork = visualizeNetwork
  )
}

read_cwl <- function(file, format = c("json", "yaml")) {
  format <- match.arg(format)
  if (format == "yaml") {
    return(yaml::read_yaml(file))
  }
  if (format == "json") {
    return(jsonlite::fromJSON(file))
  }
}

get_label <- function(x) {
  if (is.null(x$label)) {
    return(NULL)
  }
  return(x$label)
}

parse_inputs <- function(x) {
  if (is.null(x$inputs)) {
    return(NULL)
  }
  inputs <- x$inputs
  if (is.list(inputs)) inputs <- as.data.frame(dplyr::bind_rows(inputs))
  return(inputs)
}

parse_outputs <- function(x) {
  if (is.null(x$outputs)) {
    return(NULL)
  }
  
  outputs <- x$outputs
  if (is.list(outputs)) outputs <- as.data.frame(outputs)
  return(outputs)
}

parse_steps <- function(x) {
  if (is.null(x$steps)) {
    return(NULL)
  }
  # since this can be a JSON list or dict, we need to deal
  # with each case separately in downstream functions
  return(x$steps)
}

get_nodes <- function(inputs, outputs, steps) {
  # nodes - input/output nodes on the side + step nodes in the middle
  nodes <- data.frame(
    "id" = c(
      inputs$id,
      outputs$id,
      steps$id
    ),
    "label" = c(
      inputs$label,
      outputs$label,
      steps$label
    ),
    "group" = c(
      rep("input", length(inputs$id)),
      rep("output", length(outputs$id)),
      rep("step", length(steps$id))
    ),
    stringsAsFactors = FALSE
  )
  nodes$"id" <- sanitize_id(nodes$"id")
  nodes
}

edges_outputs <- function(output_source, outputs) {

  df <- data.frame(
    "from" = character(),
    "to" = character(),
    "port_from" = character(),
    "port_to" = character(),
    "type" = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:length(outputs$id)) {
    if (grepl("/", output_source[i])) {
      val_vec <- strsplit(output_source[i], "/")[[1]]
      df[i, "from"] <- val_vec[1]
      df[i, "to"] <- outputs$id[i]
      df[i, "port_from"] <- val_vec[2]
      df[i, "port_to"] <- NA
      df[i, "type"] <- "step_to_output"
    } else {
      df[i, "from"] <- output_source[i]
      df[i, "to"] <- outputs$id[i]
      df[i, "port_from"] <- NA
      df[i, "port_to"] <- NA
      df[i, "type"] <- "step_to_output"
    }
  }
  return(df)
}

edges_steps <- function(steps_in, steps) {
  df <- data.frame(
    "from" = character(),
    "to" = character(),
    "port_from" = character(),
    "port_to" = character(),
    "type" = character(),
    stringsAsFactors = FALSE
  )
  
  # replacing every null with NA so that is.na() won't give logical(0)
  for (i in 1:length(steps_in)) {
    steps_in[[i]][sapply(steps_in[[i]]$source, is.null), "source"] <- NA
  }
  
  for (i in 1:length(steps$id)) {
    for (j in 1:nrow(steps_in[[i]])) {
      key <- steps_in[[i]][j, "id"]
      key_vec <- strsplit(key, "/")[[1]]
      
      val <- unlist(steps_in[[i]][j, "source"])
      # iterate over the value list (if any) because one
      # port j of node i could receive inputs from multiple upstream ports
      for (k in 1:length(val)) {
        # only attach when the edge source is not NULL or NA
        if (!is.na(val[k])) {
          if (grepl("/", val[k])) {
            val_vec <- strsplit(val[k], "/")[[1]]
            tmp <- data.frame(
              "from" = val_vec[1],
              "to" = steps$id[i],
              "port_from" = val_vec[2],
              "port_to" = key_vec[2],
              "type" = "step_to_step",
              stringsAsFactors = FALSE
            )
          } else {
            tmp <- data.frame(
              "from" = val[k],
              "to" = steps$id[i],
              "port_from" = NA,
              "port_to" = key_vec[2],
              "type" = "input_to_step",
              stringsAsFactors = FALSE
            )
          }
          df <- rbind(df, tmp)
        }
      }
    }
  }
  return(df)
}

get_edges <- function(outputs, steps) {
  output_source <- unlist(outputs[["outputSource"]])
  df_edges_outputs <- edges_outputs(output_source, outputs)
  
  in_name <- "in"
  steps_in <- steps[[in_name]]
  df_edges_steps <- edges_steps(steps_in, steps)
  
  # combine edges from outputs and steps
  edges <- rbind(df_edges_steps, df_edges_outputs)
  
  edges
}

sanitize_id <- function(id) {
  # Remove hashtag
  idx <- which(substr(id, 1, 1) == "#")
  id[idx] <- substring(id[idx], 2)
  return(id)
}