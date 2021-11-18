#' Apply filter to determine data outliers
#'
#' @param data dataframe
#' @param Tukey_number 
#'
#' @return dataframe
sesame_street=function(data,Tukey_number){
  Wobs=data$width
  Hobs=data$wse
  Sobs=data$slope
  
  #flag and remove all data that are > n IQRs away from the upper and lower quartile (Tukey method)
  
  #calculate quartiles
  # W_IQR= quantile(Wobs,0.25,0.75)
  W_IQR = quantile(Wobs, probs=c(0.25,0.75), na.rm=TRUE)
  W_upper_outlier=W_IQR[2] + (Tukey_number* (W_IQR[2]-W_IQR[1]))
  W_lower_outlier=W_IQR[1] - (Tukey_number* (W_IQR[2]-W_IQR[1]))
  
  # H_IQR= quantile(Hobs,0.25,0.75)
  H_IQR = quantile(Hobs, probs=c(0.25,0.75), na.rm=TRUE)
  H_upper_outlier=H_IQR[2] + (Tukey_number* (H_IQR[2]-H_IQR[1]))
  H_lower_outlier=H_IQR[1] - (Tukey_number* (H_IQR[2]-H_IQR[1]))
  
  # S_IQR= quantile(Sobs,0.25,0.75)
  S_IQR = quantile(Sobs, probs=c(0.25,0.75), na.rm=TRUE)
  S_upper_outlier=S_IQR[2] + (Tukey_number* (S_IQR[2]-S_IQR[1]))
  S_lower_outlier=S_IQR[1] - (Tukey_number* (S_IQR[2]-S_IQR[1]))
  
  # Track flags as numeric 1/0 vector or matrix
  width_flags = Wobs>  W_upper_outlier | Wobs<  W_lower_outlier
  width_flags[is.na(width_flags)] = FALSE
  width_flags[width_flags == FALSE] = 0
  width_flags[width_flags == TRUE] = 1
  
  wse_flags = Hobs>  H_upper_outlier | Hobs<  H_lower_outlier
  wse_flags[is.na(wse_flags)] = FALSE
  wse_flags[wse_flags == FALSE] = 0
  wse_flags[wse_flags == TRUE] = 1
  
  slope_flags = Sobs>  S_upper_outlier | Sobs<  S_lower_outlier
  slope_flags[is.na(slope_flags)] = FALSE
  slope_flags[slope_flags == FALSE] = 0
  slope_flags[slope_flags == TRUE] = 1
  
  # Apply flags to data
  W_flagged=which(Wobs>  W_upper_outlier | Wobs<  W_lower_outlier )
  H_flagged=which(Hobs>  H_upper_outlier | Hobs<  H_lower_outlier )
  S_flagged=which(Sobs>  S_upper_outlier | Sobs<  S_lower_outlier )
  
  Wobs[W_flagged]=NA
  Hobs[H_flagged]=NA
  Sobs[S_flagged]=NA
  
  data$width=Wobs
  data$wse=Hobs
  data$slope=Sobs
  
  return(list(data=data, flags=list(width_flags=width_flags,
                                    wse_flags=wse_flags, 
                                    slope_flags=slope_flags)))
  
}

#' Identify and apply flags to reach-level data
#'
#' @param data dataframe of reach data
#' @param dark_thresh 
#' @param node_thresh 
#' @param obs_thresh 
#'
#' @return dataframe of reach data
apply_flags_reach=function(data,dark_thresh, node_thresh, obs_thresh){
  
  flag1=data$ice_clim_f
  flag2=data$ice_dyn_f
  flag3=data$dark_frac
  flag4=data$n_good_nod
  flag5=data$obs_frac_n
  
  #na will mess it up later
  flag1[is.na(flag1)]=0
  flag2[is.na(flag2)]=0
  flag3[is.na(flag3)]=0
  flag4[is.na(flag4)]=0
  flag5[is.na(flag5)]=0
  
  #make non binary flags binary
  flag3[flag3<dark_thresh]=0
  flag3[flag3>dark_thresh]=1  
  
  flag4[flag4<node_thresh]=1
  flag4[flag4>node_thresh]=0
 
  flag5[flag5<obs_thresh]=1
  flag5[flag5>obs_thresh]=0
  
  #make a giant flag
  master_flag=flag1*flag2*flag3*flag4*flag5
  
  Wobs=data$width
  Hobs=data$wse
  Sobs=data$slope
  
  Wobs[master_flag]=NA
  Hobs[master_flag]=NA
  Sobs[master_flag]=NA
  
  data$width=Wobs
  data$wse=Hobs
  data$slope=Sobs
  
  return(list(data=data, flags=list(ice_clim_f=flag1, ice_dyn_f=flag2, 
                                    dark_frac=flag3, n_good_nod=flag4, 
                                    obs_frac_n=flag5)))
}

#' Identify and apply flags to reach-level data
#'
#' @param data dataframe of node-level data
#' @param dark_thresh 
#'
#' @return dataframe of node-level data
apply_flags_node=function(data,dark_thresh){
  
  flag1=data$ice_clim_f
  flag2=data$ice_dyn_f
  flag3=data$dark_frac

  #na will mess it up later
  flag1[is.na(flag1)]=0
  flag2[is.na(flag2)]=0
  flag3[is.na(flag3)]=0
  
  #make non binary flags binary
  flag3[flag3<dark_thresh]=0
  flag3[flag3>dark_thresh]=1  
  
  #make a giant flag
  master_flag=flag1*flag2*flag3
  
  Wobs=data$width
  Hobs=data$wse
  Sobs=data$slope
  
  Wobs[master_flag]=NA
  Hobs[master_flag]=NA
  Sobs[master_flag]=NA
  
  data$width=Wobs
  data$wse=Hobs
  data$slope=Sobs
  
  return(list(data=data, flags=list(ice_clim_f=flag1, ice_dyn_f=flag2, 
                                    dark_frac=flag3)))
  
}

#' Run diagnostics on SWOT data
#'
#' @param input_dir string path to input directory
#' @param reaches_json string name of JSON reach file
#' @param tolerance float filter tolerance level
#' @param index integer used to index reaches_json file
#' @param output_dir string path to output directory
run_diagnostics <- function(input_dir, reaches_json, tolerance, index, output_dir) {
  
  # Retrieve input data
  reach_files <- get_reach_files(reaches_json, input_dir, index)
  data <- get_data(reach_files)
  
  # Check if data is valid
  width <- data$reach_list$width[!is.na(data$reach_list$width)]
  wse <- data$reach_list$wse[!is.na(data$reach_list$wse)]
  slope <- data$reach_list$slope[!is.na(data$reach_list$slope)]
  if (length(width) != 0 || length(wse) != 0 || length(slope) != 0) {
    
    # Apply flags to reach and node data
    reach_diag_data <- apply_flags_reach(data$reach_list, 1, 1, 1)
    reach_list <- reach_diag_data$data
    reach_flags <- reach_diag_data$flags
    node_diag_data <- apply_flags_node(data$node_list, 1)
    node_list <- node_diag_data$data
    node_flags <- node_diag_data$flags
    
    # Apply sesame street filter to reach and node data
    reach_ses_diags <- sesame_street(reach_list, 1.5)
    reach_list <- reach_ses_diags$data
    reach_outliers <- reach_ses_diags$flags
    node_ses_diags <- sesame_street(node_list, 1.5)
    node_list <- node_ses_diags$data
    node_outliers <- node_ses_diags$flags
    
    # Write output of diagnostics
    write_data(reach_list, node_list, reach_flags, node_flags, reach_outliers, 
               node_outliers, reach_files$swot, output_dir)
    message(paste0(reach_files$reach_id, ": Node and reach files overwritten."))
  
    } else {
    message(paste0(reach_files$reach_id, ": Invalid data; node and reach files not modified."))
  }
}