library(ncdf4)

#' Writes processed data to appropriate NetCDF file
#'
#' @param reach_list named list of reach-level data
#' @param node_list named list of node-level data
#' @param swot_file string path to swot file
write_data <- function(reach_list, node_list, swot_file) {
  
  swot <- nc_open(swot_file, write=TRUE)
  # Reach
  ncvar_put(swot, "reach/slope2", reach_list$slope)
  ncvar_put(swot, "reach/width", reach_list$width)
  ncvar_put(swot, "reach/wse", reach_list$wse)
  # Node
  ncvar_put(swot, "node/slope2", node_list$slope)
  ncvar_put(swot, "node/width", node_list$width)
  ncvar_put(swot, "node/wse", node_list$wse)
  nc_close(swot)
}
