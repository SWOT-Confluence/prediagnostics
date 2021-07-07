source("input.R")
source("output.R")

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
  W_IQR= quantile(Wobs,0.25,0.75)
  W_upper_outlier=W_IQR[2] + (Tukey_number* (W_IQR[2]-W_IQR[1]))
  W_lower_outlier=W_IQR[1] - (Tukey_number* (W_IQR[2]-W_IQR[1]))
  
  H_IQR= quantile(Hobs,0.25,0.75)
  H_upper_outlier=H_IQR[2] + (Tukey_number* (H_IQR[2]-H_IQR[1]))
  H_lower_outlier=H_IQR[1] - (Tukey_number* (H_IQR[2]-H_IQR[1]))
  
  S_IQR= quantile(Sobs,0.25,0.75)
  S_upper_outlier=S_IQR[2] + (Tukey_number* (S_IQR[2]-S_IQR[1]))
  S_lower_outlier=S_IQR[1] - (Tukey_number* (S_IQR[2]-S_IQR[1]))
  
  W_flagged=which(Wobs>  W_upper_outlier | Wobs<  W_lower_outlier )
  H_flagged=which(Hobs>  H_upper_outlier | Hobs<  H_lower_outlier )
  S_flagged=which(Sobs>  S_upper_outlier | Sobs<  S_lower_outlier )
  
  Wobs[W_flagged]=NA
  Hobs[H_flagged]=NA
  Sobs[S_flagged]=NA
  
  data$width=Wobs
  data$wse=Hobs
  data$slope=Sobs
  
  return(data)
  
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
  
  return(data)
  
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
  # Sobs=data$slope
  
  Wobs[master_flag]=NA
  Hobs[master_flag]=NA
  # Sobs[master_flag]=NA
  
  data$width=Wobs
  data$wse=Hobs
  # data$slope=Sobs
  
  return(data)
  
}

#' Run diagnostics on SWOT data
run_diagnostics <- function() {
  
  # Retrieve input data
  input_dir <- "/home/nikki/Documents/confluence/workspace/diagnostics/pre_data"   ## CHANGE ME
  args <- commandArgs(trailingOnly=TRUE)
  # reaches_json <- ifelse(is.null(args), "reaches.json", args[1])
  reaches_json <- "reaches.json"
  # index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1 ## TODO for container
  index <- 7
  reach_files <- get_reach_files(reaches_json, input_dir, index)
  data <- get_data(reach_files)
  
  # Apply flags to reach and node data
  reach_df <- apply_flags_reach(data$reach_df, 1, 1, 1)
  node_df <- apply_flags_node(data$node_df, 1)
  
  # Apply sesame street filter to reach and node data
  reach_df <- sesame_street(reach_df, 1)
  node_df <- sesame_street(node_df, 1)
  
  # Write output of diagnostics
  write_data(reach_df, node_df, reach_files)
  message("Node and reach files written.")
  
}

run_diagnostics()