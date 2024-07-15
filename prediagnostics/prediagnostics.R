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
apply_flags_reach=function(data, ice_max, dark_max, xover_cal_q_max, 
                           prior_width_min,target_bit,cross_track_dist_min_m,
                           reach_length_min_m,slope_r_u_max,wse_r_u_max ){
  #read in flags
  ice_flag=data$ice_clim_f
  dark_flag=data$dark_frac
  xover_flag=data$xovr_cal_q
  prior_width=data$prior_width
  bitwise_flag=data$bitwise
  reach_length_flag=data$reach_length
  xtrack_flag=data$cross_track_dist
  wse_u_flag=data$wse_r_u
  slope_u_flag=data$slope_r_u
    
  #convert bitwise integers to bit arrays
 bitwiser=function(bitwise_in, target_bit_in){
    bitpass=  (+(bitwAnd(target_bit_in, 2^seq(0,28))>0)) - (+(bitwAnd(bitwise_in, 2^seq(0,28))>0))
    if(any(bitpass<0)){return(0)}else{return(1)}
        }

     
  #make non binary flags binary
    #syntax below returns 1 when condition is met
    ice_flag = +(ice_flag <= ice_max)
    dark_flag = +(dark_flag <= dark_max)
    xover_flag = +(xover_flag <= xover_cal_q_max)
    prior_width_flag = +(prior_width >= prior_width_min)
    #make a matrix of bitwise filtered data
    bitwise_flag = do.call(cbind,lapply(bitwise_flag,bitwiser,target_bit_in= target_bit))
    xtrack_flag = +(xtrack_flag >= cross_track_dist_min_m)
    reach_length_flag = +(reach_length_flag >= reach_length_min_m)
    wse_u_flag = +(wse_u_flag <= wse_r_u_max)
    slope_u_flag = +(slope_u_flag <= slope_r_u_max)

 

 
  #na will mess it up later
  ice_flag[is.na(ice_flag)]=0
  dark_flag[is.na(dark_flag)]=0
  xover_flag[is.na(xover_flag)]=0
  prior_width_flag[is.na(prior_width_flag)]=0
  bitwise_flag[is.na(bitwise_flag)]=0
  xtrack_flag[is.na(xtrack_flag)]=0
  reach_length_flag[is.na(reach_length_flag)]=0
  slope_u_flag[is.na(slope_u_flag)]=0     
  wse_u_flag[is.na(wse_u_flag)]=0 
        
    ice_flag=matrix(ice_flag,nrow=1,ncol=length(ice_flag))
    dark_flag=matrix(dark_flag,nrow=1,ncol=length(dark_flag))
    xover_flag=matrix(xover_flag,nrow=1,ncol=length(xover_flag))
    prior_width_flag=matrix(prior_width_flag,nrow=1,ncol=length(prior_width_flag))
    bitwise_flag=matrix(bitwise_flag,nrow=1,ncol=length(bitwise_flag))
    xtrack_flag=matrix(xtrack_flag,nrow=1,ncol=length(xtrack_flag))
    reach_length_flag=matrix(reach_length_flag,nrow=1,ncol=length(reach_length_flag))
    wse_u_flag=matrix(wse_u_flag,nrow=1,ncol=length(wse_u_flag))
    slope_u_flag=matrix(slope_u_flag,nrow=1,ncol=length(slope_u_flag))
    
    
  
     # print(ice_flag)
     # print(dark_flag)
     # print(xover_flag)
     # print(prior_width_flag)
     # print(bitwise_flag)
     # print(xtrack_flag)
     # print(reach_length_flag)

  #we now have binary flags for 1 = KEEP, 0 = DROP
  #this is an 'or' flag, so we multiply
  # Define your flags
  flags <- list(
    ice = ice_flag,
    dark = dark_flag,
    xover = xover_flag,
    prior_width = prior_width_flag,
    bitwise = bitwise_flag,
    reach_length = reach_length_flag,
    xtrack = xtrack_flag,
    wse_u = wse_u_flag,
    slope_u = slope_u_flag
  )

  
# master_flag=ice_flag*dark_flag*xover_flag*prior_width_flag*bitwise_flag*reach_length_flag*xtrack_flag*wse_u_flag*slope_u_flag
   
#     ntot=sum(master_flag)
    
 # Iterate over flags
  master_flag <- NULL  # Initialize master_flag

  for (flag_name in names(flags)) {
    flag_value <- flags[[flag_name]]
    
    
    if (is.null(master_flag)) {
      master_flag <- flag_value
    } else {
      master_flag <- master_flag * flag_value
    }
    
    cat("master_flag after", flag_name, ":", master_flag, "\n\n")
  }
  ntot=sum(master_flag)
    
    
  Wobs=data$width
  Hobs=data$wse
  Sobs=data$slope
  S2obs=data$slope2
  
  # Find anywhere where 0 and replace with NA
  Wobs[master_flag == 0]=NA
  Hobs[master_flag == 0]=NA
  Sobs[master_flag == 0]=NA
  S2obs[master_flag == 0]=NA
  
    #rewrite
  data$width=Wobs
  data$wse=Hobs
  data$slope=Sobs
  data$slope2=S2obs

    #flags take an oppoiste meaning in broader env, so flip 0s and 1s
    
    ################################
    ####################################
    
    ###### flips from 1 = KEPT to 1= elmimnated ##########
    
    #################################
    ########################
  return(list(data=data, flags=list(ice_flag=+(!ice_flag), 
                                    prior_width_flag=+(!prior_width_flag), 
                                    dark_flag=+(!dark_flag), 
                                    reach_length_flag=+(!reach_length_flag), 
                                    xtrack_flag=+(!xtrack_flag), 
                                    bitwise_flag=+(!bitwise_flag),
                                    xover_flag=+(!xover_flag),
                                    slope_u_flag=+(!slope_u_flag),
                                    wse_u_flag =+(!wse_u_flag),
                                   master_flag = +(!master_flag),
                                   ntot= ntot)   
             ))
}

#' Identify and apply flags to reach-level data
#'
#' @param data dataframe of node-level data
#' @param dark_thresh 
#'
#' @return dataframe of node-level data
apply_flags_node=function(data, ice_max, dark_max, xover_cal_q_max, 
                           prior_width_min,target_bit,cross_track_dist_min_m, 
                          slope_r_u_max,wse_r_u_max ) {
  
  #read in flags
  ice_flag=data$ice_clim_f
  dark_flag=data$dark_frac
  xover_flag=data$xovr_cal_q
  prior_width=data$prior_width
  bitwise_flag=data$bitwise
  # reach_length_flag=data$reach_length
  xtrack_flag=data$cross_track_dist
  wse_u_flag=data$wse_r_u
  slope_u_flag=data$slope_r_u
    
 
    
   
#convert bitwise integers to bit arrays
 bitwiser_node=function(bitwise_in, target_bit_in){
    bitpass=  +(bitwAnd(target_bit_in, 2^seq(0,28))>0) - (+(bitwAnd(bitwise_in, 2^seq(0,28))>0))
     if(all(is.na(bitpass))){return(0)}
    if(any(bitpass<0)){return(0)}else{return(1)}
        }

     
  #make non binary flags binary
    #syntax below returns 1 when condition is met
    ice_flag = +(ice_flag <= ice_max)
    dark_flag = +(dark_flag <= dark_max)
    xover_flag = +(xover_flag <= xover_cal_q_max)
    prior_width_flag = +(prior_width >= prior_width_min)
    #in node mode, we need to apply bitwiser to all elements of the matrix
    bitwise_flag_node=matrix(nrow=nrow(bitwise_flag),ncol=ncol(bitwise_flag))
    for (i in 1:nrow(bitwise_flag)){
        for(j in 1:ncol(bitwise_flag)){
            this_bit= bitwiser_node(bitwise_flag[i,j],target_bit)
             bitwise_flag_node[i,j]=this_bit
            }
        }

    bitwise_flag = bitwise_flag_node
    xtrack_flag = +(xtrack_flag >= cross_track_dist_min_m)
    # reach_length_flag = +(reach_length_flag >= reach_length_min_km)
    wse_u_flag = +(wse_u_flag <= wse_r_u_max)
    slope_u_flag = +(slope_u_flag <= slope_r_u_max)
     
    
 
  #na will mess it up later
  ice_flag[is.na(ice_flag)]=0
  dark_flag[is.na(dark_flag)]=0
  xover_flag[is.na(xover_flag)]=0
  prior_width_flag[is.na(prior_width_flag)]=0
  bitwise_flag[is.na(bitwise_flag)]=0
  xtrack_flag[is.na(xtrack_flag)]=0
  # reach_length_flag[is.na(reach_length_flag)]=0
     bitwise_flag[is.na(bitwise_flag)]=0
  slope_u_flag[is.na(slope_u_flag)]=0     
  wse_u_flag[is.na(wse_u_flag)]=0  
    # print(ice_flag)
    # print(dark_flag)
    # print(xover_flag)
    # print(prior_width_flag)
    # print(bitwise_flag)
    # print(xtrack_flag)
    ## print(reach_length_flag)
#we now have binary flags for 1 = KEEP, 0 = DROP
#this is an 'or' flag, so we multiply

  master_flag=ice_flag*dark_flag*xover_flag*prior_width_flag*bitwise_flag*xtrack_flag*wse_u_flag*slope_u_flag#*reach_length_flag
  

  Wobs=data$width
  Hobs=data$wse
  Sobs=data$slope
  S2obs=data$slope2
  
  # Find anywhere where 0 and replace with NA
  Wobs[master_flag == 0]=NA
  Hobs[master_flag == 0]=NA
  Sobs[master_flag == 0]=NA
  S2obs[master_flag == 0]=NA
  
    #rewrite
  data$width=Wobs
  data$wse=Hobs
  data$slope=Sobs
  data$slope2=S2obs
  
    #flags take an oppoiste meaning in broader env, so flip 0s and 1s
    
    ################################
    ####################################
    
    ###### flips from 1 = KEPT to 1= elmimnated ##########
    
    #################################
    ########################
   
  return(list(data=data, flags=list(ice_flag=+(!ice_flag), 
                                    prior_width_flag=+(!prior_width_flag), 
                                    dark_flag=+(!dark_flag), 
                                    xtrack_flag=+(!xtrack_flag), 
                                    bitwise_flag=+(!bitwise_flag),
                                    xover_flag=+(!xover_flag),
                                    slope_u_flag=+(!slope_u_flag),
                                    wse_u_flag =+(!wse_u_flag))
             
   
             
             ))
  
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
      #edit on 7/11/24 to turn this off
    # d_x_area_flags[is.na(data$width)] <- 1
    # d_x_area_flags[is.na(data$wse)] <- 1
  } else {
    d_x_area_flags <- array(0, dim=length)
        #edit on 7/11/24 to turn this off
    # d_x_area_flags[is.na(data$width)] <- 1
    # d_x_area_flags[is.na(data$wse)] <- 1
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
    
    library(dplyr)
  
  # Retrieve input data
  reach_files <- get_reach_files(reaches_json, input_dir, index)
    
  
  data <- get_data(reach_files)
  
  # Check if data is valid
  width <- data$reach_list$width[!is.na(data$reach_list$width)]
  wse <- data$reach_list$wse[!is.na(data$reach_list$wse)]
  slope <- data$reach_list$slope[!is.na(data$reach_list$slope)]
  if (length(width) > 1 || length(wse) > 1 || length(slope) > 1) {
    
    # Apply flags to reach and node data
    
    reach_diag_data <- apply_flags_reach(data$reach_list,
                                         ice_max=GLOBAL_PARAMS$ice_max, 
                                         dark_max=GLOBAL_PARAMS$dark_max, 
                                         xover_cal_q_max=GLOBAL_PARAMS$xover_cal_q_max, 
                                         prior_width_min=GLOBAL_PARAMS$prior_width_min, 
                                         target_bit=GLOBAL_PARAMS$target_bit, 
                                         cross_track_dist_min_m=GLOBAL_PARAMS$cross_track_dist_min_m, 
                                         reach_length_min_m=GLOBAL_PARAMS$reach_length_min_m,
                                         wse_r_u_max=GLOBAL_PARAMS$wse_r_u_max,
                                         slope_r_u_max=GLOBAL_PARAMS$slope_r_u_max)
    reach_list <- reach_diag_data$data
    reach_flags <- reach_diag_data$flags
    node_diag_data <- apply_flags_node(data$node_list, 
                                         ice_max=GLOBAL_PARAMS$ice_max, 
                                         dark_max=GLOBAL_PARAMS$dark_max, 
                                         xover_cal_q_max=GLOBAL_PARAMS$xover_cal_q_max, 
                                         prior_width_min=GLOBAL_PARAMS$prior_width_min, 
                                         target_bit=GLOBAL_PARAMS$target_bit, 
                                         cross_track_dist_min_m=GLOBAL_PARAMS$cross_track_dist_min_m,
                                         wse_r_u_max=GLOBAL_PARAMS$wse_r_u_max,
                                         slope_r_u_max=GLOBAL_PARAMS$slope_r_u_max)
    node_list <- node_diag_data$data
    node_flags <- node_diag_data$flags

#       ## debugging toggle-------------
   
#       outmat=data.frame(
#           'ice'=sum(reach_flags$ice_flag)/length(reach_flags$ice_flag),
#           'p_width'=sum(reach_flags$prior_width_flag)/length(reach_flags$ice_flag),
#           'dark'=sum(reach_flags$dark_flag)/length(reach_flags$ice_flag),
#           'p_length'=sum(reach_flags$reach_length_flag)/length(reach_flags$ice_flag),
#           'xtrack'=sum(reach_flags$xtrack_flag)/length(reach_flags$ice_flag),
#           'bitwise'=sum(reach_flags$bitwise_flag)/length(reach_flags$ice_flag),
#           'xover'=sum(reach_flags$xover_flag)/length(reach_flags$ice_flag),
#           'slope_u'=sum(reach_flags$slope_u_flag)/length(reach_flags$ice_flag),
#           'wse_u'=sum(reach_flags$wse_u_flag)/length(reach_flags$ice_flag),
#           'master'=sum(reach_flags$master_flag)/length(reach_flags$ice_flag),
#           'ntot'=reach_flags$ntot,
#           'reach_id'=reach_files$reach_id)
                    

#       return( outmat)
      
#       ###end debugging toggle---------------
      

    
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
    
    # 
      
      
#output of diagnostics
    write_data(reach_list, node_list, reach_flags, node_flags, reach_outliers, 
               node_outliers, reach_slope_flags, node_slope_flags, reach_dxa_flags,
               node_dxa_flags, reach_files$swot, output_dir, GLOBAL_PARAMS, data)
    message(paste0(reach_files$reach_id, ": Node and reach files overwritten."))
  
    } else {
    message(paste0(reach_files$reach_id, ": Invalid data; node and reach files not modified."))
  }
}