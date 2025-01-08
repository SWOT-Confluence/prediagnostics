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
  json_data <- fromJSON(file=reaches_json)[[index]]
  return(list(reach_id=json_data$reach_id, 
              swot=file.path(input_dir, "swot", json_data$swot),
              sword=file.path(input_dir, "sword", json_data$sword)))
}

#' Get and store data from reach and node files
#' 
#' Data is stored in a reach-level named list and node-level named list.
#'
#' @param reach_files named list of files associated with reach
#'
#' @return named list of node named list and reach named list
get_data <- function(reach_files) {
    
 
  
  # Retrieve SWOT data
  swot <- open.nc(reach_files$swot)
  reach_list <- get_reach_data(swot, reach_files$reach_id)
  node_list <- get_node_data(swot, reach_files$reach_id)
  r_grp = grp.inq.nc(swot, "reach")$self
  time_str = var.get.nc(r_grp, "time_str")
  close.nc(swot)
    
    #
   sword_base='/nas/cee-water/cjgleason/data/SWORD/SWORDv16/netcdf/'
    bleh=reach_files$sword
    split1=strsplit(bleh,'/')[[1]][11]
    split2=paste(strsplit(split1,'_')[[1]][1:3],collapse="_")
    split3=paste0(split2,'.nc')

    reach_files$sword=paste0(sword_base,split3)
  
  # Retrieve SWORD data
  sword <- open.nc(reach_files$sword)
  r_grp = grp.inq.nc(sword, "reaches")$self
  reach_ids = var.get.nc(r_grp, "reach_id")
  sword_slope = var.get.nc(r_grp, "slope")
  low_slope_flags = var.get.nc(r_grp, "low_slope_flag")
  index = which(reach_ids == reach_files$reach_id)
  low_slope_flag = low_slope_flags[index]
  slope = sword_slope[index]/1000 # Convert m/km in SWORD to m/m for SWOT
  close.nc(sword)
  
  return(list(reach_list=reach_list, node_list=node_list, sword_slope=slope,
              low_slope_flag=low_slope_flag, time_str=time_str))
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
                    node_id = t(var.get.nc(node_grp, "node_id")),
                    slope2 = t(var.get.nc(node_grp, "slope2")),
                    slope = t(var.get.nc(node_grp, "slope")),
                    width = t(var.get.nc(node_grp, "width")), 
                    wse = t(var.get.nc(node_grp, "wse")), 
                    d_x_area = t(var.get.nc(node_grp, "d_x_area")),
                    node_q = t(var.get.nc(node_grp, "node_q")),
                    dark_frac = t(var.get.nc(node_grp, "dark_frac")),
                    ice_clim_f = t(var.get.nc(node_grp, "ice_clim_f")),
                    ice_dyn_f = t(var.get.nc(node_grp, "ice_dyn_f")),
                    # partial_f = t(var.get.nc(node_grp, "partial_f")),
                    n_good_pix = t(var.get.nc(node_grp, "n_good_pix")),
                    xovr_cal_q = t(var.get.nc(node_grp, "xovr_cal_q")),
                    cross_track_dist= abs(t(var.get.nc(node_grp, "xtrk_dist"))),
                    prior_width= t(var.get.nc(node_grp, "p_width")),
                    bitwise= t(var.get.nc(node_grp, "node_q_b")),
                    slope_r_u=t(var.get.nc(node_grp, "slope_r_u")),
                    wse_r_u=t(var.get.nc(node_grp, "wse_r_u"))
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
                    d_x_area = var.get.nc(reach_grp, "d_x_area"), 
                    slope2 = var.get.nc(reach_grp, "slope2"),
                    slope = var.get.nc(reach_grp, "slope"),
                    reach_q = var.get.nc(reach_grp, "reach_q"),
                    dark_frac = var.get.nc(reach_grp, "dark_frac"),
                    ice_clim_f = var.get.nc(reach_grp, "ice_clim_f"),
                    ice_dyn_f = var.get.nc(reach_grp, "ice_dyn_f"),
                    partial_f = var.get.nc(reach_grp, "partial_f"),
                    n_good_nod = var.get.nc(reach_grp, "n_good_nod"),
                    obs_frac_n = var.get.nc(reach_grp, "obs_frac_n"),
                    xovr_cal_q = var.get.nc(reach_grp, "xovr_cal_q"),
                    cross_track_dist= abs(t(var.get.nc(reach_grp, "xtrk_dist"))),
                    prior_width= t(var.get.nc(reach_grp, "p_width")),
                    reach_length= t(var.get.nc(reach_grp, "p_length")),
                    bitwise= t(var.get.nc(reach_grp, "reach_q_b")),
                    slope_r_u=t(var.get.nc(reach_grp, "slope_r_u")),
                    wse_r_u=t(var.get.nc(reach_grp, "wse_r_u"))
  ))
}

