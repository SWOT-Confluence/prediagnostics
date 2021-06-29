#' Write processed data to appropriate NetCDF file
#'
#' @param reach_df dataframe with reach-level data
#' @param node_df dataframe with node-level data
#' @param reach_files list of associated reach files
write_data <- function(reach_df, node_df, reach_files) {
  ## Will write new data out to appropriate reach files
  ## Considering appending versus writing new and functionality of ncdf4 library
}