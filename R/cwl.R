#' Define the cwl network manager module
#'
#' @param cwl A CWL object.
#' @param hierarchical Logical, whether to use hierarchical layout.
#' @param direction Character, direction of the hierarchical layout.
#' @param separation Numeric, level separation in the hierarchical layout.
#' @param palette Character vector, colors for input, output, and step nodes.
#' @param width Character, width of the network visualization.
#' @param height Character, height of the network visualization.
#' @return A visNetwork object.
#' @export
#' @examples
#' visualizeNetwork(cwl)
visualizeNetwork <- function(cwl, hierarchical = TRUE, direction = "LR", separation = 300,
                             palette = c("#C3C3C3", "#FF8F00", "#00AAA8"),
                             width = "300%", height = "%100") {
  nodes <- get_nodes(
    cwl %>% parse_inputs(),
    cwl %>% parse_outputs(),
    cwl %>% parse_steps()
  )
  edges <- get_edges(
    cwl %>% parse_outputs(),
    cwl %>% parse_steps()
  )
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

#' Read CWL from file
#'
#' @param file Character, path to the CWL file.
#' @param format Character, format of the CWL file ("json" or "yaml").
#' @return A list representing the CWL content.
#' @export
#' @examples
#' read_cwl("path/to/file.cwl")
read_cwl <- function(file, format = c("json", "yaml")) {
  format <- ifelse(!is.list(format), format, tools::file_ext(file))
  format <- match.arg(format)
  if (format == "yaml") {
    return(yaml::read_yaml(file))
  }
  if (format == "json") {
    return(jsonlite::fromJSON(file))
  }
}

#' Get label from CWL object
#'
#' @param x A CWL object.
#' @return Character, the label of the CWL object.
#' @export
#' @examples
#' get_label(cwl_object)
get_label <- function(x) {
  if (is.null(x$label)) {
    return(NULL)
  }
  return(x$label)
}

#' Parse inputs from CWL object
#'
#' @param x A CWL object.
#' @return Data frame of inputs.
#' @export
#' @examples
#' parse_inputs(cwl_object)
parse_inputs <- function(x) {
  if (is.null(x$inputs)) {
    return(NULL)
  }
  inputs <- x$inputs
  if (is.list(inputs)) inputs <- as.data.frame(dplyr::bind_rows(inputs))
  return(inputs)
}

#' Parse outputs from CWL object
#'
#' @param x A CWL object.
#' @return Data frame of outputs.
#' @export
#' @examples
#' parse_outputs(cwl_object)
parse_outputs <- function(x) {
  if (is.null(x$outputs)) {
    return(NULL)
  }
  
  outputs <- x$outputs
  if (is.list(outputs)) outputs <- as.data.frame(outputs)
  return(outputs)
}

#' Parse steps from CWL object
#'
#' @param x A CWL object.
#' @return List of steps.
#' @export
#' @examples
#' parse_steps(cwl_object)
parse_steps <- function(x) {
  if (is.null(x$steps)) {
    return(NULL)
  }
  return(x$steps)
}

#' Get nodes for network visualization
#'
#' @param inputs Data frame of inputs.
#' @param outputs Data frame of outputs.
#' @param steps List of steps.
#' @return Data frame of nodes.
#' @export
#' @examples
#' get_nodes(inputs, outputs, steps)
get_nodes <- function(inputs, outputs, steps) {
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

#' Get edges from outputs
#'
#' @param output_source Character vector of output sources.
#' @param outputs Data frame of outputs.
#' @return Data frame of edges.
#' @export
#' @examples
#' edges_outputs(output_source, outputs)
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

#' Get edges from steps
#'
#' @param steps_in List of step inputs.
#' @param steps List of steps.
#' @return Data frame of edges.
#' @export
#' @examples
#' edges_steps(steps_in, steps)
edges_steps <- function(steps_in, steps) {
  df <- data.frame(
    "from" = character(),
    "to" = character(),
    "port_from" = character(),
    "port_to" = character(),
    "type" = character(),
    stringsAsFactors = FALSE
  )
  
  for (i in 1:length(steps_in)) {
    steps_in[[i]][sapply(steps_in[[i]]$source, is.null), "source"] <- NA
  }
  
  for (i in 1:length(steps$id)) {
    for (j in 1:nrow(steps_in[[i]])) {
      key <- steps_in[[i]][j, "id"]
      key_vec <- strsplit(key, "/")[[1]]
      
      val <- unlist(steps_in[[i]][j, "source"])
      for (k in 1:length(val)) {
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

#' Get edges for network visualization
#'
#' @param outputs Data frame of outputs.
#' @param steps List of steps.
#' @return Data frame of edges.
#' @export
#' @examples
#' get_edges(outputs, steps)
get_edges <- function(outputs, steps) {
  output_source <- unlist(outputs[["outputSource"]])
  df_edges_outputs <- edges_outputs(output_source, outputs)
  
  in_name <- "in"
  steps_in <- steps[[in_name]]
  df_edges_steps <- edges_steps(steps_in, steps)
  
  edges <- rbind(df_edges_steps, df_edges_outputs)
  
  edges
}

#' Sanitize node IDs
#'
#' @param id Character vector of node IDs.
#' @return Character vector of sanitized node IDs.
#' @export
#' @examples
#' sanitize_id(c("#node1", "#node2"))

sanitize_id <- function(id) {
  # Remove hashtag
  idx <- which(substr(id, 1, 1) == "#")
  id[idx] <- substring(id[idx], 2)
  return(id)
}