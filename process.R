source("input.R")
source("output.R")

#' Retrieves SWOT observation data
#' 
#' Stores data in a named list organized into reach-level data and node-level
#' data.
#' 
#' @param input_dir string path to directory that contains input data
#'
#' @return named list of reach and node dataframes
get_input <- function(input_dir) {
  
  args <- commandArgs(trailingOnly=TRUE)
  # reaches_json <- ifelse(is.null(args), "reaches.json", args[1])
  reaches_json <- "reaches.json"
  # index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1 ## TODO for container
  index <- 7
  
  reach_files <- get_reach_files(reaches_json, input_dir, index)
  return(get_data(reach_files))
}

#' Run diagnostics on SWOT data
#'
#' @param data named list of reach and node dataframes
#'
#' @return named list of reach and node dataframes
run_diagnostics <- function(data) {
  ## Do Stuff
}

# Run the program
input_dir <- "/home/nikki/Documents/confluence/workspace/diagnostics/data"    # CHANGE ME
data <- get_input(input_dir)
data <- run_diagnostics(input_dir)
write_data(data)