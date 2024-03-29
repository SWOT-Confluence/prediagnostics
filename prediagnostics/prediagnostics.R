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
  S2obs=data$slope2
  
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
  
  S2_IQR = quantile(S2obs, probs=c(0.25,0.75), na.rm=TRUE)
  S2_upper_outlier=S2_IQR[2] + (Tukey_number* (S2_IQR[2]-S2_IQR[1]))
  S2_lower_outlier=S2_IQR[1] - (Tukey_number* (S2_IQR[2]-S2_IQR[1]))
  
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
  
  slope2_flags = S2obs>  S2_upper_outlier | S2obs<  S2_lower_outlier
  slope2_flags[is.na(slope2_flags)] = FALSE
  slope2_flags[slope2_flags == FALSE] = 0
  slope2_flags[slope2_flags == TRUE] = 1
  
  # Apply flags to data
  W_flagged=which(Wobs>  W_upper_outlier | Wobs<  W_lower_outlier )
  H_flagged=which(Hobs>  H_upper_outlier | Hobs<  H_lower_outlier )
  S_flagged=which(Sobs>  S_upper_outlier | Sobs<  S_lower_outlier )
  S2_flagged=which(S2obs>  S2_upper_outlier | S2obs<  S2_lower_outlier )
  
  Wobs[W_flagged]=NA
  Hobs[H_flagged]=NA
  Sobs[S_flagged]=NA
  S2obs[S2_flagged]=NA
  
  data$width=Wobs
  data$wse=Hobs
  data$slope=Sobs
  data$slope2=S2obs
  
  return(list(data=data, flags=list(width_flags=width_flags,
                                    wse_flags=wse_flags, 
                                    slope_flags=slope_flags,
                                    slope2_flags=slope2_flags)))
  
}

#' Identify and apply flags to reach-level data
#'
#' @param data dataframe of reach data
#' @param dark_thresh 
#' @param node_thresh 
#' @param obs_thresh 
#' @param reachq_thresh
#' @param xovr_thresh
#'
#' @return dataframe of reach data
apply_flags_reach=function(data, ice_thresh, dark_thresh, obs_thresh, 
                           reachq_thresh, xovr_thresh){
  
  flag1=data$ice_clim_f
  # flag2=data$ice_dyn_f
  flag3=data$dark_frac
  flag5=data$obs_frac_n
  flag6=data$reach_q
  flag7=data$xovr_cal_q
  
  #make non binary flags binary
  flag1 <- +(flag1 <= ice_thresh)
  # flag2 <- +(flag2 <= ice_thresh)
  flag3 <- +(flag3 <= dark_thresh)
  flag5 <- +(flag5 >= obs_thresh)
  flag6 <- +(flag6 <= reachq_thresh)
  flag7 <- +(flag7 <= xovr_thresh)
  
  #na will mess it up later
  flag1[is.na(flag1)]=0
  # flag2[is.na(flag2)]=0
  flag3[is.na(flag3)]=0
  flag5[is.na(flag5)]=0
  flag6[is.na(flag6)]=0
  flag7[is.na(flag7)]=0
  
  #make a giant flag
  # master_flag=flag1*flag2*flag3*flag5*flag6*flag7
  master_flag=flag1*flag3*flag5*flag6*flag7
  
  Wobs=data$width
  Hobs=data$wse
  Sobs=data$slope
  S2obs=data$slope2
  
  # Find anywhere where 0 and replace with NA
  Wobs[master_flag == 0]=NA
  Hobs[master_flag == 0]=NA
  Sobs[master_flag == 0]=NA
  S2obs[master_flag == 0]=NA
  
  data$width=Wobs
  data$wse=Hobs
  data$slope=Sobs
  data$slope2=S2obs
  
  return(list(data=data, flags=list(ice_clim_f=+(!flag1), 
                                    # ice_dyn_f=+(!flag2), 
                                    dark_frac=+(!flag3), 
                                    obs_frac_n=+(!flag5), 
                                    reach_q=+(!flag6),
                                    xovr_cal_q=+(!flag7))))
}

#' Identify and apply flags to reach-level data
#'
#' @param data dataframe of node-level data
#' @param dark_thresh 
#'
#' @return dataframe of node-level data
apply_flags_node=function(data, ice_thresh, dark_thresh, nodeq_thresh, 
                          xovr_thresh) {
  
  flag1 = data$ice_clim_f
  # flag2 = data$ice_dyn_f
  flag3 = data$dark_frac
  flag4 = data$node_q
  flag5 = data$xovr_cal_q
  
  #make non binary flags binary
  flag1 = +(flag1 <= ice_thresh)
  # flag2 = +(flag2 <= ice_thresh)
  flag3 = +(flag3 <= dark_thresh)
  flag4 = +(flag4 <= nodeq_thresh) 
  flag5 = +(flag5 <= xovr_thresh)
  
  #na will mess it up later
  flag1[is.na(flag1)] = 0
  # flag2[is.na(flag2)] = 0
  flag3[is.na(flag3)] = 0
  flag4[is.na(flag4)] = 0
  flag5[is.na(flag5)] = 0
  
  #make a giant flag
  # master_flag = flag1*flag2*flag3*flag4*flag5
  master_flag = flag1*flag3*flag4*flag5
  
  Wobs = data$width
  Hobs = data$wse
  Sobs = data$slope
  S2obs = data$slope2
  
  # Find anywhere where 0 and replace with NA
  Wobs[master_flag == 0] = NA
  Hobs[master_flag == 0] = NA
  Sobs[master_flag == 0] = NA
  S2obs[master_flag == 0] = NA
  
  data$width = Wobs
  data$wse = Hobs
  data$slope = Sobs
  data$slope2 = S2obs
  
  return(list(data = data, flags = list(ice_clim_f = +(!flag1), 
                                        # ice_dyn_f = +(!flag2),
                                        dark_frac = +(!flag3),
                                        node_q = +(!flag4),
                                        xovr_cal_q = +(!flag5))))
  
}

#' Apply filter to filter out low slope
#'
#' @param data dataframe
#' @param min_slope 
#'
#' @return dataframe
low_slope=function(data, sword_slope, min_slope, level){
  
  # Determine if prior slope is larger than constant
  if (sword_slope > min_slope) {
    slope_value <- sword_slope
  } else {
    slope_value <- min_slope
  }
  
  # Set slope and slope2
  length = dim(data$slope)
  if (level == "reach") {
    data$slope <- rep(slope_value, times=c(length))
    data$slope2 <- rep(slope_value, times=length)
    slope_flags <- rep(1, times=length)
  } else {
    data$slope <- array(slope_value, dim=length)
    data$slope2 <- array(slope_value, dim=length)
    slope_flags <- array(1, dim=length)
  }
  
  return(list(data=data, flags=slope_flags))
  
}

#' Apply filter to d_x_area to mask out values where there is not any width or
#' wse data present.
#'
#' @param data dataframe
#'
#' @return dataframe
filter_dxa=function(data, level){
  
  # Filter width an wse NAs
  data$d_x_area[is.na(data$width)] <- NA
  data$d_x_area[is.na(data$wse)] <- NA
  
  # Save flags to indicate overwritten data
  length = dim(data$width)
  if (level == "reach") {
    d_x_area_flags <- rep(0, times=c(length))
    d_x_area_flags[is.na(data$width)] <- 1
    d_x_area_flags[is.na(data$wse)] <- 1
  } else {
    d_x_area_flags <- array(0, dim=length)
    d_x_area_flags[is.na(data$width)] <- 1
    d_x_area_flags[is.na(data$wse)] <- 1
  }
  
  return(list(data=data, flags=d_x_area_flags))
  
}

#' Run diagnostics on SWOT data
#'
#' @param input_dir string path to input directory
#' @param reaches_json string name of JSON reach file
#' @param index integer used to index reaches_json file
#' @param output_dir string path to output directory
run_diagnostics <- function(input_dir, reaches_json, index, output_dir) {
  
  # Retrieve input data
  reach_files <- get_reach_files(reaches_json, input_dir, index)
  data <- get_data(reach_files)
  
  # Check if data is valid
  width <- data$reach_list$width[!is.na(data$reach_list$width)]
  wse <- data$reach_list$wse[!is.na(data$reach_list$wse)]
  slope <- data$reach_list$slope[!is.na(data$reach_list$slope)]
  if (length(width) != 0 || length(wse) != 0 || length(slope) != 0) {
    
    # Apply flags to reach and node data
    reach_diag_data <- apply_flags_reach(data$reach_list,
                                         GLOBAL_PARAMS$reach_ice,
                                         GLOBAL_PARAMS$reach_dark, 
                                         GLOBAL_PARAMS$reach_obs,
                                         GLOBAL_PARAMS$reach_q,
                                         GLOBAL_PARAMS$reach_xovr_cal_q)
    reach_list <- reach_diag_data$data
    reach_flags <- reach_diag_data$flags
    node_diag_data <- apply_flags_node(data$node_list, 
                                       GLOBAL_PARAMS$node_ice,
                                       GLOBAL_PARAMS$node_dark,
                                       GLOBAL_PARAMS$node_q, 
                                       GLOBAL_PARAMS$node_xovr_cal_q)
    node_list <- node_diag_data$data
    node_flags <- node_diag_data$flags
    
    # Apply sesame street filter to reach and node data
    reach_ses_diags <- sesame_street(reach_list, GLOBAL_PARAMS$Tukey_number)
    reach_list <- reach_ses_diags$data
    reach_outliers <- reach_ses_diags$flags
    node_ses_diags <- sesame_street(node_list, GLOBAL_PARAMS$Tukey_number)
    node_list <- node_ses_diags$data
    node_outliers <- node_ses_diags$flags
    
    # Apply low slope filter to reach and node data
    if (data$low_slope_flag == 1) {
      reach_low_slope_diags <- low_slope(reach_list, data$sword_slope, GLOBAL_PARAMS$slope_min, "reach")
      reach_list <- reach_low_slope_diags$data
      reach_slope_flags <- reach_low_slope_diags$flags
      node_low_slope_diags <- low_slope(node_list, data$sword_slope, GLOBAL_PARAMS$slope_min, "node")
      node_list <- node_low_slope_diags$data
      node_slope_flags <- node_low_slope_diags$flags
    } else {
      reach_slope_flags <- rep(0, times=dim(data$reach_list$slope))
      node_slope_flags <- array(0, dim=dim(data$node_list$slope))
    }
    
    # Filter d_x_area based on width and wse
    reach_dxa_diags <- filter_dxa(reach_list, "reach")
    reach_list <- reach_dxa_diags$data
    reach_dxa_flags <- reach_dxa_diags$flags
    node_dxa_diags <- filter_dxa(node_list, "node")
    node_list <- node_dxa_diags$data
    node_dxa_flags <- node_dxa_diags$flags
    
    # Write output of diagnostics
    write_data(reach_list, node_list, reach_flags, node_flags, reach_outliers, 
               node_outliers, reach_slope_flags, node_slope_flags, reach_dxa_flags,
               node_dxa_flags, reach_files$swot, output_dir)
    message(paste0(reach_files$reach_id, ": Node and reach files overwritten."))
  
    } else {
    message(paste0(reach_files$reach_id, ": Invalid data; node and reach files not modified."))
  }
}