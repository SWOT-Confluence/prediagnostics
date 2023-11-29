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
write_data <- function(reach_list, node_list, reach_flags, node_flags, 
                       reach_outliers, node_outliers, reach_slope_flags, 
                       node_slope_flags, swot_file, output_dir) {
  
  # Update SWOT files
  update_swot(swot_file, reach_list, node_list)
  
  # Record results of prediagnostics
  record_results(output_dir, reach_list$reach_id, reach_flags, node_flags, 
                 reach_outliers, node_outliers, reach_slope_flags, 
                 node_slope_flags)
  
}

#' Updates processed data to appropriate NetCDF file
#'
#' @param swot_file string path to swot file
#' @param reach_list named list of reach-level data
#' @param node_list named list of node-level data
update_swot <- function(swot_file, reach_list, node_list) {
  swot <- open.nc(swot_file, write=TRUE)
  # Reach
  reach_grp = grp.inq.nc(swot, "reach")$self
  var.put.nc(reach_grp, "slope", reach_list$slope)
  var.put.nc(reach_grp, "slope2", reach_list$slope2)
  var.put.nc(reach_grp, "width", reach_list$width)
  var.put.nc(reach_grp, "wse", reach_list$wse)
  # Node
  node_grp = grp.inq.nc(swot, "node")$self
  var.put.nc(node_grp, "slope", t(node_list$slope))
  var.put.nc(node_grp, "slope2", t(node_list$slope2))
  var.put.nc(node_grp, "width", t(node_list$width))
  var.put.nc(node_grp, "wse", t(node_list$wse))
  close.nc(swot)
}

#' Records results of prediagnostic flagging operations.
#'
#' @param output_dir string path to output directory
#' @param reach_id integer reach identifier
#' @param reach_outliers named list of reach-level outliers
#' @param node_outliers named list of node-level outliers
#' @param reach_slope_flags array of reach-level low slope flags
#' @param node_slope_flags array of node-level low slope flags
record_results <- function(output_dir, reach_id, reach_flags, node_flags,
                           reach_outliers, node_outliers, reach_slope_flags,
                           node_slope_flags) {
  
  nc_file = paste(output_dir, paste0(reach_id, "_prediagnostics.nc"), sep=.Platform$file.sep)
  nc_out = create.nc(nc_file, format="netcdf4")
  
  # Global attr
  att.put.nc(nc_out, "NC_GLOBAL", "reach_id", "NC_INT64", reach_id)
  
  # Dims and coord vars
  dim.def.nc(nc_out, "num_nodes", dim(node_flags$ice_clim_f)[1])
  var.def.nc(nc_out, "num_nodes", "NC_INT", "num_nodes")
  att.put.nc(nc_out, "num_nodes", "units", "NC_STRING", "number of nodes")
  var.put.nc(nc_out, "num_nodes", c(1:dim(node_flags$ice_clim_f)[1]))
  
  dim.def.nc(nc_out, "time_steps", dim(node_flags$ice_clim_f)[2])
  var.def.nc(nc_out, "time_steps", "NC_INT", "time_steps")
  att.put.nc(nc_out, "time_steps", "units", "NC_STRING", "number of observations")
  var.put.nc(nc_out, "time_steps", c(1:dim(node_flags$ice_clim_f)[2]))
  
  # Groups and variables
  fill = -999
  r_grp = grp.def.nc(nc_out, "reach")
  write_reach_flags(r_grp, reach_flags, reach_outliers, reach_slope_flags, fill)
  
  n_grp = grp.def.nc(nc_out, "node")
  write_node_flags(n_grp, node_flags, node_outliers, node_slope_flags, fill)
  
  close.nc(nc_out)
  
}

#' Record reach-level flags
#'
#' @param r_grp NetCDF reach group
#' @param reach_flags named list of reach-level flags
#' @param reach_outliers named list of reach-level outliers
#' @param reach_slope_flags array of reach-level low slope flags
#' @param fill integer missing value
write_reach_flags <- function(r_grp, reach_flags, reach_outliers, 
                              reach_slope_flags, fill) {
  
  var.def.nc(r_grp, "ice_clim_f", "NC_INT", "time_steps")
  att.put.nc(r_grp, "ice_clim_f", "long_name", "NC_STRING", "climatological ice cover flag")
  att.put.nc(r_grp, "ice_clim_f", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "ice_clim_f", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "ice_clim_f", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "ice_clim_f", reach_flags$ice_clim_f)
  
  var.def.nc(r_grp, "ice_dyn_f", "NC_INT", "time_steps")
  att.put.nc(r_grp, "ice_dyn_f", "long_name", "NC_STRING", "dynamical ice cover flag")
  att.put.nc(r_grp, "ice_dyn_f", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "ice_dyn_f", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "ice_dyn_f", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "ice_dyn_f", reach_flags$ice_dyn_f)
  
  var.def.nc(r_grp, "dark_frac", "NC_INT", "time_steps")
  att.put.nc(r_grp, "dark_frac", "long_name", "NC_STRING", "Fraction of reach area_total covered by dark water")
  att.put.nc(r_grp, "dark_frac", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "dark_frac", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "dark_frac", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "dark_frac", reach_flags$dark_frac)
  
  var.def.nc(r_grp, "n_good_nod", "NC_INT", "time_steps")
  att.put.nc(r_grp, "n_good_nod", "long_name", "NC_STRING", "number of nodes in the reach that have a valid WSE")
  att.put.nc(r_grp, "n_good_nod", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "n_good_nod", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "n_good_nod", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "n_good_nod", reach_flags$n_good_nod)
  
  var.def.nc(r_grp, "obs_frac_n", "NC_INT", "time_steps")
  att.put.nc(r_grp, "obs_frac_n", "long_name", "NC_STRING", "fraction of nodes that have a valid WSE")
  att.put.nc(r_grp, "obs_frac_n", "flag_values", "NC_STRING", "0 1")
  att.put.nc(r_grp, "obs_frac_n", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(r_grp, "obs_frac_n", "_FillValue", "NC_INT", fill)
  var.put.nc(r_grp, "obs_frac_n", reach_flags$obs_frac_n)
  
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
  
}

#' Record node-level flags
#'
#' @param n_grp NetCDF node group
#' @param node_flags named list of node-level flags
#' @param node_outliers named list of node-level outliers
#' @param node_slope_flags array node-level low slope flags
#' @param fill integer missing value
write_node_flags <- function(n_grp, node_flags, node_outliers, node_slope_flags,
                             fill) {
  
  var.def.nc(n_grp, "ice_clim_f", "NC_INT", c("num_nodes", "time_steps"))
  att.put.nc(n_grp, "ice_clim_f", "long_name", "NC_STRING", "climatological ice cover flag")
  att.put.nc(n_grp, "ice_clim_f", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "ice_clim_f", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "ice_clim_f", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "ice_clim_f", node_flags$ice_clim_f)
  
  var.def.nc(n_grp, "ice_dyn_f", "NC_INT", c("num_nodes", "time_steps"))
  att.put.nc(n_grp, "ice_dyn_f", "long_name", "NC_STRING", "dynamical ice cover flag")
  att.put.nc(n_grp, "ice_dyn_f", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "ice_dyn_f", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "ice_dyn_f", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "ice_dyn_f", node_flags$ice_dyn_f)
  
  var.def.nc(n_grp, "dark_frac", "NC_INT", c("num_nodes", "time_steps"))
  att.put.nc(n_grp, "dark_frac", "long_name", "NC_STRING", "Fraction of reach area_total covered by dark water")
  att.put.nc(n_grp, "dark_frac", "flag_values", "NC_STRING", "0 1")
  att.put.nc(n_grp, "dark_frac", "flag_meanings", "NC_STRING", "not_overwritten overwritten")
  att.put.nc(n_grp, "dark_frac", "_FillValue", "NC_INT", fill)
  var.put.nc(n_grp, "dark_frac", node_flags$dark_frac)
  
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
  
}
