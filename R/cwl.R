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
                             font = c(color = "#343434", size = 18, "face" = "arial", "strokeWidth" = 2, "strokeColor" = "#DEE2E6"),
                             width = "300%", height = "100%"){
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
    visNodes(borderWidth = 2, font = font) %>%
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
    ) %>%
    visOptions(highlightNearest = TRUE,
               nodesIdSelection = list(
                 enabled = TRUE,
                 style = 'width: 150px; height: 26px'
    ))
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
#' @param target_step The target step after which the new step will be added.
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
#' updated_flow <- append_step(flow, new_step, "target_step")
#'
#' # Save the updated workflow back to a JSON file
#' write_cwl(updated_flow, "path_to_updated_cwl_workflow.json", "json", pretty = TRUE)
#' }
#'
#' @export
append_step <- function(flow, step, target_step = NULL) {
  
  # Function to extract suffix before "in_" or "out_"
  extract_suffix <- function(id, pattern) {
    sub(pattern, "", id)
  }
  
  # Extract the inputs, outputs and steps
  step_inputs <- step %>% parse_inputs()
  step_outputs <- step %>% parse_outputs()
  step_steps <- step %>% parse_steps()
  
  flow_inputs <- flow %>% parse_inputs()
  flow_outputs <- flow %>% parse_outputs()
  flow_steps <- flow %>% parse_steps()
  
  if (!is.null(target_step) && target_step != "") {
    # Find the position of the target step
    target_position <- which.max(flow_steps$id == target_step)
    # Handle inputs
    new_inputs <- data.frame()
    for (i in 1:nrow(step_inputs)) {
      input_id <- step_inputs[i, ]$id
      input_suffix <- extract_suffix(input_id, "in_")
      # Check if the input matches the output of any existing step
      found_match <- FALSE
      j_step_id <- flow_steps[target_position, ]$id
      j_step_out <- as.data.frame(flow_steps[target_position, ]$out)
      k <- 1
      while(!found_match && (k <= nrow(j_step_out))) {
        output_id <- j_step_out[k, ]
        output_suffix <- extract_suffix(output_id, "out_")
        if (input_suffix == output_suffix) {
          step_inputs[i, ]$source <- paste(j_step_id, output_id, sep = "/")
          found_match <- TRUE
          break
        }
        k <- k + 1
      }
      # If no match found, check if the input matches an existing input in the workflow
      j_input <- flow_inputs[target_position, ]
      if (input_id == j_input$id) {
        step_inputs[i, ]$source <- j_input$id
        found_match <- TRUE
      }
      # If still no match found, append the input to the workflow
      if (!found_match) {
        new_inputs <- rbind(new_inputs, step_inputs[i, ])
      }
    }
    flow$inputs <- rbind(flow_inputs, new_inputs)
    # Handle outputs
    for (i in 1:nrow(step_outputs)) {
      output_id <- step_outputs[i, ]$id
      
      # Check if the output already exists in the workflow
      output_exists <- any(flow_outputs$id == output_id)
      
      # If not, append the output to the workflow
      if (!output_exists) {
        flow$outputs <- rbind(flow_outputs, step_outputs[i, ])
      }
    }
  } else {
    # Just append to the end of the flow
    target_position <- nrow(flow_steps)
    
    # Handle inputs
    new_inputs <- data.frame()
    for (i in 1:nrow(step_inputs)) {
      input_id <- step_inputs[i, ]$id
      input_suffix <- extract_suffix(input_id, "in_")
      # Check if the input matches the output of any existing step
      found_match <- FALSE
      j <- 1
      while(!found_match && (j <= nrow(flow_steps))) {
        j_step_id <- flow_steps[j, ]$id
        j_step_out <- as.data.frame(flow_steps[j, ]$out)
        k <- 1
        while(!found_match && (k <= nrow(j_step_out))) {
          output_id <- j_step_out[k, ]
          output_suffix <- extract_suffix(output_id, "out_")
          if (input_suffix == output_suffix) {
            step_inputs[i, ]$source <- paste(j_step_id, output_id, sep = "/")
            found_match <- TRUE
            break
          }
          k <- k + 1
        }
        if (found_match) break
        j <- j + 1
      }
      # If no match found, check if the input matches an existing input in the workflow
      j <- 1
      while(!found_match && (j <= nrow(flow_inputs))) {
        j_input <- flow_inputs[j, ]
        if (input_id == j_input$id) {
          step_inputs[i, ]$source <- j_input$id
          found_match <- TRUE
          break
        }
        j <- j + 1
      }
      # If still no match found, append the input to the workflow
      if (!found_match) {
        new_inputs <- rbind(new_inputs, step_inputs[i, ])
      }
    }
    flow$inputs <- rbind(flow_inputs, new_inputs)
    # Handle outputs
    for (i in 1:nrow(step_outputs)) {
      output_id <- step_outputs[i, ]$id
      
      # Check if the output already exists in the workflow
      output_exists <- any(flow_outputs$id == output_id)
      
      # If not, append the output to the workflow
      if (!output_exists) {
        flow$outputs <- rbind(flow_outputs, step_outputs[i, ])
      }
    }
  }
  # # Generate a hash from the input step id
  # hash <- digest(step$id, algo = "md5")
  # 
  # # Convert the hash to a numeric value and take the first 3 digits
  # hash_numeric <- as.numeric(substr(gsub("[^0-9]", "", hash), 1, 3))
  # 
  # # Append the step at the target position
  # step_steps$id <- paste(step_steps$id, hash_numeric, sep = "_")
  # Split flow_steps at the target position
  if (target_position < 1) {
    flow_steps_l <- data.frame()
  } else {
    flow_steps_l <- flow_steps %>% slice(1:target_position)
  }
  
  if (target_position + 1 > nrow(flow_steps)) {
    flow_steps_r <- data.frame()
  } else {
    flow_steps_r <- flow_steps %>% slice((target_position + 1):n())
  }
  # Combine the dataframes
  flow$steps <- rbind(flow_steps_l, step_steps, flow_steps_r)

  return(list(cwl=flow, step_id = step_steps$id))
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
  if (is.list(inputs)) inputs <- as.data.frame(inputs)
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
  steps <- x$steps
  if (is.list(steps)) steps <- as.data.frame(steps)
  return(steps)
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
  
  i <- 1
  while (i <= length(outputs$id)) {
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
    i <- i + 1
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