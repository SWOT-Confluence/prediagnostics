library(RNetCDF)

#' Writes processed data to appropriate NetCDF file
#'
#' @param reach_list named list of reach-level data
#' @param node_list named list of node-level data
#' @param reach_flags named list of reach-level flags
#' @param node_flags named list of node-level flags
#' @param reach_outliers named list of reach-level outliers
#' @param node_outliers named list of node-level outliers
#' @param reach_slope_flags array of reach-level low slope flags
#' @param node_slope_flags array of node-level low slope flags
#' @param swot_file string path to swot file
#' @param output_dir string path to output directory
#' @param reach_dxa_flags Indicates where d_x_area has been overwritten with NA
#' @param node_dxa_flags Indicates where d_x_area has been overwritten with NA
write_data <- function(reach_list, node_list, reach_flags, node_flags, 
                       reach_outliers, node_outliers, reach_slope_flags, 
                       node_slope_flags, reach_dxa_flags, node_dxa_flags, 
                       swot_file, output_dir, GLOBAL_PARAMS, data) {
  

    # print('data to be written')
    # print(node_list$slope2)
    # bonk
  # Update SWOT files

  update_swot(swot_file, reach_list, node_list)
    
  
  # Record results of prediagnostics
record_results(output_dir, reach_list$reach_id, reach_flags, node_flags, 
                 reach_outliers, node_outliers, reach_slope_flags, 
                 node_slope_flags, reach_dxa_flags, node_dxa_flags, data)
  
}

#' Updates processed data to appropriate NetCDF file
#'
#' @param swot_file string path to swot file
#' @param reach_list named list of reach-level data
#' @param node_list named list of node-level data
update_swot <- function(swot_file, reach_list, node_list) {
    
# print('slope that is written to netcdf')
    # print(reach_list$slope)



    
  swot <- open.nc(swot_file, write=TRUE)
  # Reach
  reach_grp = grp.inq.nc(swot, "reach")$self
  var.put.nc(reach_grp, "slope", reach_list$slope)
  var.put.nc(reach_grp, "slope2", reach_list$slope2)
  var.put.nc(reach_grp, "width", reach_list$width)
  var.put.nc(reach_grp, "wse", reach_list$wse)
  var.put.nc(reach_grp, "d_x_area", reach_list$d_x_area)
  # Node
  node_grp = grp.inq.nc(swot, "node")$self
  var.put.nc(node_grp, "slope", t(node_list$slope))
  var.put.nc(node_grp, "slope2", t(node_list$slope2))
  var.put.nc(node_grp, "width", t(node_list$width))
  var.put.nc(node_grp, "wse", t(node_list$wse))
  var.put.nc(node_grp, "d_x_area", t(node_list$d_x_area))
  close.nc(swot)
}

# Function to add global attributes to the NetCDF file
add_global_attributes <- function(nc, params) {
  for (name in names(params)) {
    value <- params[[name]]
    if (is.integer(value)) {
      att.put.nc(nc, "NC_GLOBAL", name, "NC_INT", as.integer(value))
    } else if (is.numeric(value)) {
      att.put.nc(nc, "NC_GLOBAL", name, "NC_DOUBLE", as.double(value))
    } else if (is.character(value)) {
      att.put.nc(nc, "NC_GLOBAL", name, "NC_CHAR", value)
    }
  }
}

#' Records results of prediagnostic flagging operations.
#'
#' @param output_dir string path to output directory
#' @param reach_id integer reach identifier
#' @param reach_outliers named list of reach-level outliers
#' @param node_outliers named list of node-level outliers
#' @param reach_slope_flags array of reach-level low slope flags
#' @param node_slope_flags array of node-level low slope flags
#' @param reach_dxa_flags Indicates where d_x_area has been overwritten with NA
#' @param node_dxa_flags Indicates where d_x_area has been overwritten with NA 
record_results <- function(output_dir, reach_id, reach_flags, node_flags,
                           reach_outliers, node_outliers, reach_slope_flags,
                           node_slope_flags, reach_dxa_flags, node_dxa_flags, data) {
  
  nc_file = paste(output_dir, paste0(reach_id, "_prediagnostics.nc"), sep=.Platform$file.sep)
  nc_out = create.nc(nc_file, format="netcdf4")
    
   
  
  # Global attr
  att.put.nc(nc_out, "NC_GLOBAL", "reach_id", "NC_INT64", as.integer(reach_id))
  add_global_attributes(nc_out, GLOBAL_PARAMS)
   # print(dim(node_flags$ice_flag)[2])

  # Dims and coord vars
  dim.def.nc(nc_out, "num_nodes", dim(node_flags$ice_flag)[1])
  var.def.nc(nc_out, "num_nodes", "NC_INT", "num_nodes")
  att.put.nc(nc_out, "num_nodes", "units", "NC_STRING", "number of nodes")
  var.put.nc(nc_out, "num_nodes", c(1:dim(node_flags$ice_flag)[1]))
  
  dim.def.nc(nc_out, "time_steps", dim(data$time_str)[2])
  var.def.nc(nc_out, "time_steps", "NC_STRING", "time_steps")
  att.put.nc(nc_out, "time_steps", "units", "NC_STRING", "number of observations")
  var.put.nc(nc_out, "time_steps", as.vector(data$time_str))
  
  # Groups and variables
  fill = -999
  r_grp = grp.def.nc(nc_out, "reach")
  write_reach_flags(r_grp, reach_flags, reach_outliers, reach_slope_flags, reach_dxa_flags, fill)
  
  n_grp = grp.def.nc(nc_out, "node")
  write_node_flags(n_grp, node_flags, node_outliers, node_slope_flags, node_dxa_flags, fill)
  
  close.nc(nc_out)
  
}

#' Record reach-level flags
#'
#' @param r_grp NetCDF reach group
#' @param reach_flags named list of reach-level flags
#' @param reach_outliers named list of reach-level outliers
#' @param reach_slope_flags array of reach-level low slope flags
#' @param reach_dxa_flags Indicates where d_x_area has been overwritten with NA
#' @param fill integer missing value
write_reach_flags <- function(r_grp, reach_flags, reach_outliers, 
                              reach_slope_flags, reach_dxa_flags, fill) {
  


  var.def.nc(r_grp, "ice_clim_f", "NC_INT", "time_steps")
  att.put.nc(r_grp, "ice_clim_f", "long_name", "NC_STRING", "climatological ice cover flag")
  att.put.nc(r_grp, "ice_clim_f", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "ice_clim_f", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "ice_clim_f", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "ice_clim_f", as.vector(reach_flags$ice_flag))
  
  var.def.nc(r_grp, "dark_frac", "NC_INT", "time_steps")
  att.put.nc(r_grp, "dark_frac", "long_name", "NC_STRING", "Fraction of reach area_total covered by dark water")
  att.put.nc(r_grp, "dark_frac", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "dark_frac", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "dark_frac", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "dark_frac",as.vector(reach_flags$dark_flag))
  
  var.def.nc(r_grp, "x_trk_dist_flag", "NC_INT", "time_steps")
  att.put.nc(r_grp, "x_trk_dist_flag", "long_name", "NC_STRING", "data within a threshold of cross track distance")
  att.put.nc(r_grp, "x_trk_dist_flag", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "x_trk_dist_flag", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "x_trk_dist_flag", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "x_trk_dist_flag",as.vector(reach_flags$xtrack_flag))
  
  var.def.nc(r_grp, "reach_q_b", "NC_INT", "time_steps")
  att.put.nc(r_grp, "reach_q_b", "long_name", "NC_STRING", "summary quality indicator for the reach")
  att.put.nc(r_grp, "reach_q_b", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "reach_q_b", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "reach_q_b", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "reach_q_b",as.vector(reach_flags$bitwise_flag))
  

  # DID NOT FIND FLAG
  var.def.nc(r_grp, "xovr_cal_q", "NC_INT", "time_steps")
  att.put.nc(r_grp, "xovr_cal_q", "long_name", "NC_STRING", "quality of the cross-over calibration")
  att.put.nc(r_grp, "xovr_cal_q", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "xovr_cal_q", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "xovr_cal_q", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "xovr_cal_q",as.vector(reach_flags$xover_flag))
    
  var.def.nc(r_grp, "prior_width_flag", "NC_INT", "time_steps")
  att.put.nc(r_grp, "prior_width_flag", "long_name", "NC_STRING", "filter for reaches meeting a minimum width")
  att.put.nc(r_grp, "prior_width_flag", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "prior_width_flag", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "prior_width_flag", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "prior_width_flag",as.vector(reach_flags$prior_width_flag))
    
  var.def.nc(r_grp, "reach_length_flag", "NC_INT", "time_steps")
  att.put.nc(r_grp, "reach_length_flag", "long_name", "NC_STRING", "filter for reaches meeting minimum length")
  att.put.nc(r_grp, "reach_length_flag", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "reach_length_flag", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "reach_length_flag", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "reach_length_flag",as.vector(reach_flags$reach_length_flag))
    
    #dummy variable for hotfix December 2024.---------
    #Remove when module is repiped-------------------
    reach_flags$wse_u_flag = reach_flags$reach_length_flag
    reach_flags$wse_u_flag[reach_flags$wse_u_flag==0] = fill
    reach_flags$wse_u_flag[reach_flags$wse_u_flag==1] = fill
    
  var.def.nc(r_grp, "wse_r_u_flag", "NC_INT", "time_steps")
  att.put.nc(r_grp, "wse_r_u_flag", "long_name", "NC_STRING", "swot height uncertainty flag")
  att.put.nc(r_grp, "wse_r_u_flag", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "wse_r_u_flag", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "wse_r_u_flag", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "wse_r_u_flag",as.vector(reach_flags$wse_u_flag))
    
    reach_flags$slope_u_flag = reach_flags$reach_length_flag
    reach_flags$slope_u_flag[reach_flags$slope_u_flag==0] = fill
    reach_flags$slope_u_flag[reach_flags$slope_u_flag==1] = fill
    
  var.def.nc(r_grp, "slope_r_u_flag", "NC_INT", "time_steps")
  att.put.nc(r_grp, "slope_r_u_flag", "long_name", "NC_STRING", "swot slope uncertainty flag")
  att.put.nc(r_grp, "slope_r_u_flag", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "slope_r_u_flag", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "slope_r_u_flag", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "slope_r_u_flag",as.vector(reach_flags$slope_u_flag))
      
    #dummy variable for hotfix December 2024.---------
    #End remove section when module is repiped-------------------
    
  var.def.nc(r_grp, "width_outliers", "NC_INT", "time_steps")
  att.put.nc(r_grp, "width_outliers", "long_name", "NC_STRING", "Outliers detected in width observations.")
  att.put.nc(r_grp, "width_outliers", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "width_outliers", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "width_outliers", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "width_outliers", reach_outliers$width_flags)
  
  var.def.nc(r_grp, "wse_outliers", "NC_INT", "time_steps")
  att.put.nc(r_grp, "wse_outliers", "long_name", "NC_STRING", "Outliers detected in wse observations.")
  att.put.nc(r_grp, "wse_outliers", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "wse_outliers", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "wse_outliers", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "wse_outliers", reach_outliers$wse_flags)
  
  var.def.nc(r_grp, "slope_outliers", "NC_INT", "time_steps")
  att.put.nc(r_grp, "slope_outliers", "long_name", "NC_STRING", "Outliers detected in slope observations.")
  att.put.nc(r_grp, "slope_outliers", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "slope_outliers", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "slope_outliers", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "slope_outliers", reach_outliers$slope_flags)
  
  var.def.nc(r_grp, "slope2_outliers", "NC_INT", "time_steps")
  att.put.nc(r_grp, "slope2_outliers", "long_name", "NC_STRING", "Outliers detected in slope2 observations.")
  att.put.nc(r_grp, "slope2_outliers", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "slope2_outliers", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "slope2_outliers", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "slope2_outliers", reach_outliers$slope2_flags)
  
  var.def.nc(r_grp, "low_slope_flag", "NC_INT", "time_steps")
  att.put.nc(r_grp, "low_slope_flag", "long_name", "NC_STRING", "Low slope flag indicator for slope and slope2 observations.")
  att.put.nc(r_grp, "low_slope_flag", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "low_slope_flag", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "low_slope_flag", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "low_slope_flag", reach_slope_flags)
  
  var.def.nc(r_grp, "d_x_area_flag", "NC_INT", "time_steps")
  att.put.nc(r_grp, "d_x_area_flag", "long_name", "NC_STRING", "Indicates if d_x_area flag was overwritten with missing value because width or wse was not observed.")
  att.put.nc(r_grp, "d_x_area_flag", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "d_x_area_flag", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "d_x_area_flag", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "d_x_area_flag", reach_dxa_flags)
  
}

#' Record node-level flags
#'
#' @param n_grp NetCDF node group
#' @param node_flags named list of node-level flags
#' @param node_outliers named list of node-level outliers
#' @param node_slope_flags array node-level low slope flags
#' @param node_dxa_flags Indicates where d_x_area has been overwritten with NA
#' @param fill integer missing value
write_node_flags <- function(n_grp, node_flags, node_outliers, node_slope_flags,
                             node_dxa_flags, fill) {

  # print(node_flags$ice_flag)

  var.def.nc(n_grp, "ice_clim_f", "NC_INT", c("num_nodes","time_steps"))
  att.put.nc(n_grp, "ice_clim_f", "long_name", "NC_STRING", "climatological ice cover flag")
  att.put.nc(n_grp, "ice_clim_f", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "ice_clim_f", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "ice_clim_f", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "ice_clim_f", node_flags$ice_flag)

  var.def.nc(n_grp, "dark_frac", "NC_INT", c("num_nodes","time_steps"))
  att.put.nc(n_grp, "dark_frac", "long_name", "NC_STRING", "Fraction of reach area_total covered by dark water")
  att.put.nc(n_grp, "dark_frac", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "dark_frac", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "dark_frac", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "dark_frac", node_flags$dark_flag)

  var.def.nc(n_grp, "x_trk_dist_flag", "NC_INT", c("num_nodes","time_steps"))
  att.put.nc(n_grp, "x_trk_dist_flag", "long_name", "NC_STRING", "data within a threshold of cross track distance")
  att.put.nc(n_grp, "x_trk_dist_flag", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "x_trk_dist_flag", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "x_trk_dist_flag", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "x_trk_dist_flag", node_flags$xtrack_flag)

  var.def.nc(n_grp, "node_q_b", "NC_INT", c("num_nodes","time_steps"))
  att.put.nc(n_grp, "node_q_b", "long_name", "NC_STRING", "summary quality indicator for the reach")
  att.put.nc(n_grp, "node_q_b", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "node_q_b", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "node_q_b", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "node_q_b", node_flags$bitwise_flag)

  var.def.nc(n_grp, "xovr_cal_q", "NC_INT", c("num_nodes","time_steps"))
  att.put.nc(n_grp, "xovr_cal_q", "long_name", "NC_STRING", "quality of the cross-over calibration")
  att.put.nc(n_grp, "xovr_cal_q", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "xovr_cal_q", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "xovr_cal_q", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "xovr_cal_q", node_flags$xover_flag)
  
  var.def.nc(n_grp, "prior_width_flag", "NC_INT", c("num_nodes","time_steps"))
  att.put.nc(n_grp, "prior_width_flag", "long_name", "NC_STRING", "filter for reaches meeting a minimum width")
  att.put.nc(n_grp, "prior_width_flag", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "prior_width_flag", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "prior_width_flag", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "prior_width_flag", node_flags$prior_width_flag)
  
  var.def.nc(n_grp, "width_outliers", "NC_INT", c("num_nodes", "time_steps"))
  att.put.nc(n_grp, "width_outliers", "long_name", "NC_STRING", "Outliers detected in width observations.")
  att.put.nc(n_grp, "width_outliers", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "width_outliers", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "width_outliers", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "width_outliers", node_outliers$width_flags)

  var.def.nc(n_grp, "wse_outliers", "NC_INT", c("num_nodes", "time_steps"))
  att.put.nc(n_grp, "wse_outliers", "long_name", "NC_STRING", "Outliers detected in wse observations.")
  att.put.nc(n_grp, "wse_outliers", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "wse_outliers", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "wse_outliers", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "wse_outliers", node_outliers$wse_flags)

  var.def.nc(n_grp, "slope_outliers", "NC_INT", c("num_nodes", "time_steps"))
  att.put.nc(n_grp, "slope_outliers", "long_name", "NC_STRING", "Outliers detected in slope observations.")
  att.put.nc(n_grp, "slope_outliers", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "slope_outliers", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "slope_outliers", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "slope_outliers", node_outliers$slope_flags)

  var.def.nc(n_grp, "slope2_outliers", "NC_INT", c("num_nodes", "time_steps"))
  att.put.nc(n_grp, "slope2_outliers", "long_name", "NC_STRING", "Outliers detected in slope2 observations.")
  att.put.nc(n_grp, "slope2_outliers", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "slope2_outliers", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "slope2_outliers", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "slope2_outliers", node_outliers$slope2_flags)

  var.def.nc(n_grp, "low_slope_flag", "NC_INT", c("num_nodes", "time_steps"))
  att.put.nc(n_grp, "low_slope_flag", "long_name", "NC_STRING", "Low slope flag indicator for slope and slope2 observations.")
  att.put.nc(n_grp, "low_slope_flag", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "low_slope_flag", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "low_slope_flag", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "low_slope_flag", node_slope_flags)

  var.def.nc(n_grp, "d_x_area_flag", "NC_INT", c("num_nodes", "time_steps"))
  att.put.nc(n_grp, "d_x_area_flag", "long_name", "NC_STRING", "Indicates if d_x_area flag was overwritten with missing value because width or wse was not observed.")
  att.put.nc(n_grp, "d_x_area_flag", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "d_x_area_flag", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "d_x_area_flag", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "d_x_area_flag", node_dxa_flags)
                             }