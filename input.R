library(RNetCDF)
library(rjson)

#' Get reach files for a reach identifier
#' 
#' The reach identifier is determined from the index number which is used to 
#' select an element from the JSON list.  
#'
#' @param reaches_json string path to JSON file with reach data
#' @param input_dir string path to input directory
#' @param index integer index for JSON file
#'
#' @return named list of reach files associate with reach identifier
get_reach_files <- function(reaches_json, input_dir, index) {
  json_data <- fromJSON(file=file.path(input_dir, reaches_json))[[index]]
  return(list(reach_id=json_data$reach_id, 
              swot=file.path(input_dir, "swot", json_data$swot)))
}

#' Get and store data from reach and node files
#' 
#' Data is stored in a reach-level named list and node-level named list.
#'
#' @param reach_files named list of files associated with reach
#'
#' @return named list of node named list and reach named list
get_data <- function(reach_files) {
  
  swot <- open.nc(reach_files$swot)
  reach_list <- get_reach_data(swot, reach_files$reach_id)
  node_list <- get_node_data(swot, reach_files$reach_id)
  close.nc(swot)
  
  return(list(reach_list=reach_list, node_list=node_list))
}

#' Retrieve node data from swot_file
#'
#' @param swot ncdf4 dataset
#' @param reach_id int unique identifier for reach
#'
#' @return named list of node data
get_node_data <- function(swot, reach_id) {
  
  node_grp = grp.inq.nc(swot, "node")$self
  return(list(reach_id = reach_id, 
                    node_id = var.get.nc(node_grp, "node_id"),
                    slope = var.get.nc(node_grp, "slope2"),
                    width = var.get.nc(node_grp, "width"), 
                    wse = var.get.nc(node_grp, "wse"), 
                    node_q = var.get.nc(node_grp, "node_q"),
                    dark_frac = var.get.nc(node_grp, "dark_frac"),
                    ice_clim_f = var.get.nc(node_grp, "ice_clim_f"),
                    ice_dyn_f = var.get.nc(node_grp, "ice_dyn_f"),
                    partial_f = var.get.nc(node_grp, "partial_f"),
                    n_good_pix = var.get.nc(node_grp, "n_good_pix"),
                    xovr_cal_q = var.get.nc(node_grp, "xovr_cal_q")
  ))
  
}

#' Retrieve reach data from swot_file
#'
#' @param swot ncdf4 dataset
#' @param reach_id int unique identifier for reach
#'
#' @return named list of reach data
get_reach_data <- function(swot, reach_id) {
  
  reach_grp = grp.inq.nc(swot, "reach")$self
  return(list(reach_id = reach_id, 
                    width = var.get.nc(reach_grp, "width"), 
                    wse = var.get.nc(reach_grp, "wse"), 
                    slope = var.get.nc(reach_grp, "slope2"),
                    reach_q = var.get.nc(reach_grp, "reach_q"),
                    dark_frac = var.get.nc(reach_grp, "dark_frac"),
                    ice_clim_f = var.get.nc(reach_grp, "ice_clim_f"),
                    ice_dyn_f = var.get.nc(reach_grp, "ice_dyn_f"),
                    partial_f = var.get.nc(reach_grp, "partial_f"),
                    n_good_nod = var.get.nc(reach_grp, "n_good_nod"),
                    obs_frac_n = var.get.nc(reach_grp, "obs_frac_n"),
                    xovr_cal_q = var.get.nc(reach_grp, "xovr_cal_q")
  ))
}
