library(RNetCDF)

#' Writes processed data to appropriate NetCDF file
#'
#' @param reach_list named list of reach-level data
#' @param node_list named list of node-level data
#' @param swot_file string path to swot file
write_data <- function(reach_list, node_list, swot_file) {
  
  swot <- open.nc(swot_file, write=TRUE)
  # Reach
  reach_grp = grp.inq.nc(swot, "reach")$self
  var.put.nc(reach_grp, "slope2", reach_list$slope)
  var.put.nc(reach_grp, "width", reach_list$width)
  var.put.nc(reach_grp, "wse", reach_list$wse)
  # Node
  node_grp = grp.inq.nc(swot, "node")$self
  var.put.nc(node_grp, "slope2", t(node_list$slope))
  var.put.nc(node_grp, "width", t(node_list$width))
  var.put.nc(node_grp, "wse", t(node_list$wse))
  close.nc(swot)
}
