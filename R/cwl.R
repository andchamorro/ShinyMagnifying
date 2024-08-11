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
#' read_cwl("path_to_your_cwl_workflow.cwl")
read_cwl <- function(file_path, format = c("json", "yaml")) {
  format <- ifelse(!is.list(format), format, tools::file_ext(file_path))
  format <- match.arg(format)
  if (format == "yaml") {
    return(yaml::read_yaml(file_path))
  }
  if (format == "json") {
    return(jsonlite::fromJSON(file_path))
  }
}

#' Write CWL to file
#'
#' This function writes the given CWL content to a file in either JSON or YAML format.
#' It uses the jsonlite package for JSON and the yaml package for YAML.
#'
#' @param cwl_content A list representing the CWL content to be written.
#' @param file_path Character, path to the output CWL file.
#' @param format Character, format of the CWL file ("json" or "yaml").
#' @return None
#' @export
#' @examples
#' cwl_content <- list(
#'   cwlVersion = "v1.2",
#'   class = "Workflow",
#'   label = "Basic Workflow",
#'   # ... other CWL content ...
#' )
#' write_cwl(cwl_content, "path_to_your_cwl_workflow.json", "json")
write_cwl <- function(cwl_content, file_path, format = c("json", "yaml"), ...) {
  format <- ifelse(!is.list(format), format, tools::file_ext(file_path))
  format <- match.arg(format)
  if (format == "json") {
    jsonlite::write_json(cwl_content, file_path, ...)
  } else if (format == "yaml") {
    yaml::write_yaml(cwl_content, file_path, ...)
  } else {
    stop("Invalid format. Supported formats: 'json' or 'yaml'")
  }
}

#' Append a Step to a CWL Workflow
#'
#' This function appends a new step to an existing CWL workflow represented as a JSON list.
#' It checks the inputs and outputs of the step against the existing workflow.
#' If an input matches the suffix of an existing step's output (before "out_" and "in_" respectively),
#' it links the input to the appropriate source. If an input matches an existing input in the workflow,
#' it links it to that input. If a new input or output is needed, it appends it to the workflow.
#'
#' @param flow A list representing the current CWL workflow in JSON format.
#' @param step A list representing the new step (in JSON format) to be added to the workflow.
#' @param target_position An integer specifying the position in the workflow where the new step should be added.
#'
#' @return A list representing the updated CWL workflow in JSON format with the new step added.
#'
#' @examples
#' \dontrun{
#' # Load an existing CWL workflow from a JSON file
#' flow <- read_cwl("path_to_your_cwl_workflow.json")
#' 
#' # Define a new step to add to the workflow
#' new_step <- list(
#'   id = "new_step_id",
#'   in = list(list(id = "input_param_in_1")),
#'   out = list(list(id = "output_param_out_1")),
#'   run = list(
#'     class = "CommandLineTool",
#'     baseCommand = "echo",
#'     arguments = list("Hello, CWL!"),
#'     inputs = list(),
#'     outputs = list(list(id = "output_file", type = "stdout")),
#'     stdout = "output.txt"
#'   )
#' )
#'
#' # Append the new step at the 3rd position in the workflow
#' updated_flow <- append_step_cwl(flow, new_step, 3)
#'
#' # Save the updated workflow back to a JSON file
#' write_cwl(updated_flow, "path_to_updated_cwl_workflow.json", "json", pretty = TRUE)
#' }
#'
#' @export
append_step_cwl <- function(flow, step, target_position) {
  # Check if the target_position is within the valid range
  if (target_position < 1 || target_position > length(flow$steps) + 1) {
    stop("Invalid target position.")
  }
  
  # Extract the inputs and outputs from the step
  step_inputs <- step$inputs
  step_outputs <- step$outputs
  
  # Function to extract suffix before "in_" or "out_"
  extract_suffix <- function(id, pattern) {
    sub(pattern, "", id)
  }
  
  # Handle inputs
  for (i in seq_along(step_inputs)) {
    input_id <- step_inputs[[i]]$id
    input_suffix <- extract_suffix(input_id, "in_.*$")
    
    # Check if the input matches the output of any existing step
    found_match <- FALSE
    for (j in seq_along(flow$steps)) {
      for (k in seq_along(flow$steps[[j]]$out)) {
        output_id <- flow$steps[[j]]$out[[k]]$id
        output_suffix <- extract_suffix(output_id, "out_.*$")
        
        if (input_suffix == output_suffix) {
          step_inputs[[i]]$source <- paste(flow$steps[[j]]$id, output_id, sep = "/")
          found_match <- TRUE
          break
        }
      }
      if (found_match) break
    }
    
    # If no match found, check if the input matches an existing input in the workflow
    if (!found_match) {
      for (j in seq_along(flow$inputs)) {
        if (input_id == flow$inputs[[j]]$id) {
          step_inputs[[i]]$source <- flow$inputs[[j]]$id
          found_match <- TRUE
          break
        }
      }
    }
    
    # If still no match found, append the input to the workflow
    if (!found_match) {
      flow$inputs <- append(flow$inputs, list(step_inputs[[i]]))
    }
  }
  
  # Handle outputs
  for (i in seq_along(step_outputs)) {
    output_id <- step_outputs[[i]]$id
    
    # Check if the output already exists in the workflow
    output_exists <- any(sapply(flow$outputs, `[[`, "id") == output_id)
    
    # If not, append the output to the workflow
    if (!output_exists) {
      flow$outputs <- append(flow$outputs, list(step_outputs[[i]]))
    }
  }
  
  # Append the step at the target position
  flow$steps <- append(flow$steps, list(step), after = target_position - 1)
  
  return(flow)
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

#' Get edges for network visualization
#'
#' @param outputs Data frame of outputs.
#' @param steps List of steps.
#' @return Data frame of edges.
#' @export
#' @examples
#' get_edges(outputs, steps)
get_edges <- function(outputs, steps) {
  out_src_name <- "outputSource"
  
  output_source <- unlist(outputs[[out_src_name]])
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