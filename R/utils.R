# Utility functions
sourceDir <- function(path, trace = TRUE, ...) {
  op <- options(); on.exit(options(op)) # to reset after each
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
    options(op)
  }
}

drop_na <- function(df) {
  df[rowSums(is.na(df)) != ncol(df), ]
}

unnest_wider <- function(df, cols, names_sep = ".") {
  # Check if cols is a character vector
  if (!is.character(cols)) {
    stop("cols must be a character vector of column names.")
  }
  
  # Initialize the result dataframe with non-list columns
  result <- df[ , !(names(df) %in% cols)]
  
  # Loop through each specified list-column
  for (col in cols) {
    list_col <- df[[col]]
    
    # Ensure every element in the list-column is named and consistent
    if (!all(sapply(list_col, function(x) all(names(x) == names(list_col[[1]]))))) {
      stop("All elements in the list-column must have the same names.")
    }
    
    # Create a new dataframe by binding the list elements as columns
    new_cols <- do.call(rbind, lapply(list_col, as.data.frame))
    
    # Rename the new columns by combining outer and inner names
    colnames(new_cols) <- paste(col, colnames(new_cols), sep = names_sep)
    
    # Combine the new columns with the result dataframe
    result <- cbind(result, new_cols)
  }
  
  return(result)
}