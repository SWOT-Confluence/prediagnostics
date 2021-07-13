library(ncdf4)
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
  
  swot <- nc_open(reach_files$swot)
  reach_list <- get_reach_data(swot, reach_files$reach_id)
  node_list <- get_node_data(swot, reach_files$reach_id)
  nc_close(swot)
  
  return(list(reach_list=reach_list, node_list=node_list))
}

#' Retrieve node data from swot_file
#'
#' @param swot ncdf4 dataset
#' @param reach_id int unique identifier for reach
#'
#' @return named list of node data
get_node_data <- function(swot, reach_id) {
  
  return(list(reach_id = reach_id, 
                    node_id = ncvar_get(swot, "node/node_id"),
                    slope = t(ncvar_get(swot, "node/slope2")),
                    width = t(ncvar_get(swot, "node/width")), 
                    wse = t(ncvar_get(swot, "node/wse")), 
                    node_q = t(ncvar_get(swot, "node/node_q")),
                    dark_frac = t(ncvar_get(swot, "node/dark_frac")),
                    ice_clim_f = t(ncvar_get(swot, "node/ice_clim_f")),
                    ice_dyn_f = t(ncvar_get(swot, "node/ice_dyn_f")),
                    partial_f = t(ncvar_get(swot, "node/partial_f")),
                    n_good_pix = t(ncvar_get(swot, "node/n_good_pix")),
                    xovr_cal_q = t(ncvar_get(swot, "node/xovr_cal_q"))
  ))
  
}

#' Retrieve reach data from swot_file
#'
#' @param swot ncdf4 dataset
#' @param reach_id int unique identifier for reach
#'
#' @return named list of reach data
get_reach_data <- function(swot, reach_id) {
  
  return(list(reach_id = reach_id, 
                    width = ncvar_get(swot, "reach/width"), 
                    wse = ncvar_get(swot, "reach/wse"), 
                    slope = ncvar_get(swot, "reach/slope2"),
                    reach_q = ncvar_get(swot, "reach/reach_q"),
                    dark_frac = ncvar_get(swot, "reach/dark_frac"),
                    ice_clim_f = ncvar_get(swot, "reach/ice_clim_f"),
                    ice_dyn_f = ncvar_get(swot, "reach/ice_dyn_f"),
                    partial_f = ncvar_get(swot, "reach/partial_f"),
                    n_good_nod = ncvar_get(swot, "reach/n_good_nod"),
                    obs_frac_n = ncvar_get(swot, "reach/obs_frac_n"),
                    xovr_cal_q = ncvar_get(swot, "reach/xovr_cal_q")
  ))
}
