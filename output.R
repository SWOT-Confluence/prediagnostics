library(ncdf4)

#' Writes processed data to appropriate NetCDF file
#'
#' @param reach_list named list of reach-level data
#' @param node_list named list of node-level data
#' @param reach_files list of associated reach files
write_data <- function(reach_list, node_list, reach_files) {
  
  write_reach_data(reach_list, reach_files$swot_reach, reach_files$reach_id)
  write_node_data(node_list, reach_files$swot_node, reach_files$reach_id)
  
}

#' Open SWOT files and overwrite appropriate SWOT data variables
#'
#' @param node_list named list of node-level data
#' @param swot string path to swot file
#' @param reach_id double reach identifier
write_node_data <- function(node_list, swot, reach_id) {
  
  node <- nc_open(swot, write=TRUE)
  reach_ids <- ncvar_get(node, "reach_id")
  indexes <- which(reach_ids==reach_id, arr.ind=TRUE)
  
  slope <- ncvar_get(node, "slope2")
  slope[,indexes] <- node_list$slope
  ncvar_put(node, "slope2", slope)
  
  width <- ncvar_get(node, "width")
  width[,indexes] <- node_list$width
  ncvar_put(node, "width", width)
  
  wse <- ncvar_get(node, "wse")
  wse[,indexes] <- node_list$wse
  ncvar_put(node, "wse", wse)
  
  nc_close(node)
  
}

#' Open SWOT files and overwrite appropriate SWOT data variables
#'
#' @param reach_list named list of reach-level data
#' @param swot string path to swot file
#' @param reach_id double reach identifier
write_reach_data <- function(reach_list, swot, reach_id) {
  
  reach <- nc_open(swot, write=TRUE)
  reach_ids <- ncvar_get(reach, "reach_id")
  index <- which(reach_ids==reach_id, arr.ind=TRUE)
  
  slope <- ncvar_get(reach, "slope2")
  slope[,index] <- reach_list$slope
  ncvar_put(reach, "slope2", slope)
  
  width <- ncvar_get(reach, "width")
  width[,index] <- reach_list$width
  ncvar_put(reach, "width", width)
  
  wse <- ncvar_get(reach, "wse")
  wse[,index] <- reach_list$wse
  ncvar_put(reach, "wse", wse)
  
  nc_close(reach)
}