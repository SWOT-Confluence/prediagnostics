library(ncdf4)
library(rjson)

#' Get reach files for a reach identifier
#' 
#' The reach identifier is determined from the Batch array index number which 
#' is used to select an element from the JSON list. 
#'
#' @param reaches_json string path to JSON file with reach data
#' @param input_dir string path to input directory
#' @param index integer index for JSON file
#'
#' @return named list of reach files associate with reach identifier
get_reach_files <- function(reaches_json, input_dir, index) {
  json_data <- fromJSON(file=paste(input_dir, reaches_json, sep='\\'))[[index]]
  return(list(reach_id=json_data$reach_id, 
              swot_reach=paste(input_dir, "swot", json_data$swot_reach, sep='\\'),
              swot_node=paste(input_dir, "swot", json_data$swot_node, sep='\\'), 
              sos=paste(input_dir, "sos", json_data$sos, sep='\\')
  ))
}

#' Get and store data from reach and node files
#' 
#' Data is stored in a reach-level dataframe and node-level dataframe.
#'
#' @param reach_files named list of files associated with reach
#'
#' @return named list of node dataframe and reach dataframe
get_data <- function(reach_files) {
  
  reach_df <- get_reach_data(reach_files$swot_reach, reach_files$reach_id)
  node_df <- get_node_data(reach_files$swot_node, reach_files$reach_id)
  
  return(list(reach_df=reach_df, node_df=node_df))
}

#' Retrieve node data from node_file using reach_id as an index
#'
#' @param node_file string path to node file
#' @param reach_id integer reach identifier
#'
#' @return dataframe of node data
get_node_data <- function(node_file, reach_id) {
  
  # Open node file and get index of reach identifier
  node <- nc_open(node_file)
  reach_ids <- ncvar_get(node, "reach_id")
  indexes <- which(reach_ids==reach_id, arr.ind=TRUE)
  
  # Use index to retrieve corresponding data variables
  node_ids <- ncvar_get(node, "node_id")[indexes]
  width <- ncvar_get(node, "width")[indexes]
  wse <- ncvar_get(node, "wse")[indexes]
  node_q <- ncvar_get(node, "node_q")[indexes]
  dark_frac <- ncvar_get(node, "dark_frac")[indexes]
  ice_clim_f <- ncvar_get(node, "ice_clim_f")[indexes]
  ice_dyn_f <- ncvar_get(node, "ice_dyn_f")[indexes]
  partial_f <- ncvar_get(node, "partial_f")[indexes]
  n_good_pix <- ncvar_get(node, "n_good_pix")[indexes]
  xovr_cal_q <- ncvar_get(node, "xovr_cal_q")[indexes]
  
  return(data.frame(reach_id = reach_id, 
                    node_ids = node_ids,
                    width = width, 
                    wse = wse, 
                    node_q = node_q,
                    dark_frac = dark_frac,
                    ice_clim_f = ice_clim_f,
                    ice_dyn_f = ice_dyn_f,
                    partial_f = partial_f,
                    n_good_pix = n_good_pix,
                    xovr_cal_q = xovr_cal_q
  ))
  
}

#' Retrieve reach data from reach_file using reach_id as an index
#'
#' @param reach_file string path to reach file
#' @param reach_id integer reach identifier
#'
#' @return dataframe of reach data
get_reach_data <- function(reach_file, reach_id) {
  # Open reach file and get index of reach identifier
  reach <- nc_open(reach_file)
  reach_ids <- ncvar_get(reach, "reach_id")
  index <- which(reach_ids==reach_id, arr.ind=TRUE)
  
  # Use index to retrieve corresponding data variables
  width <- ncvar_get(reach, "width")[index]
  wse <- ncvar_get(reach, "wse")[index]
  slope <- ncvar_get(reach, "slope2")[index]
  reach_q <- ncvar_get(reach, "reach_q")[index]
  dark_frac <- ncvar_get(reach, "dark_frac")[index]
  ice_clim_f <- ncvar_get(reach, "ice_clim_f")[index]
  ice_dyn_f <- ncvar_get(reach, "ice_dyn_f")[index]
  partial_f <- ncvar_get(reach, "partial_f")[index]
  n_good_nod <- ncvar_get(reach, "n_good_nod")[index]
  obs_frac_n <- ncvar_get(reach, "obs_frac_n")[index]
  xovr_cal_q <- ncvar_get(reach, "xovr_cal_q")[index]
  
  return(data.frame(reach_id = reach_id, 
                    width = width, 
                    wse = wse, 
                    slope = slope,
                    reach_q = reach_q,
                    dark_frac = dark_frac,
                    ice_clim_f = ice_clim_f,
                    ice_dyn_f = ice_dyn_f,
                    partial_f = partial_f,
                    n_good_nod = n_good_nod,
                    obs_frac_n = obs_frac_n,
                    xovr_cal_q = xovr_cal_q
  ))
}
