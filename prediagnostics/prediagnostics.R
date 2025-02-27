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
    
#     print('inside sesame street')
#     print(slope2_flags)
  
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
                           prior_width_min,target_bit_reach,cross_track_dist_min_m,
                           cross_track_dist_max_m,reach_length_min_m,
                           obs_frac_min){ 
    #deprecated inputs ,slope_r_u_max,wse_r_u_max  v0002
    
        hydrochronna=which(is.na(data$width))
  #read in flags
  ice_flag=data$ice_clim_f
  dark_flag=data$dark_frac
  xover_flag=data$xovr_cal_q
  prior_width=data$prior_width
  bitwise_flag=data$bitwise
  reach_length_flag=data$reach_length
  xtrack_flag=data$cross_track_dist
  obs_frac_flag = data$obs_frac_n
    
    
    
    
  # deprecated v0002  
  # wse_u_flag=data$wse_r_u
  # slope_u_flag=data$slope_r_u
    
  #convert bitwise integers to bit arrays
 bitwiser=function(bitwise_in, target_bit_in){
     #here, we do a bitwise and and turn it into a binary (that is the +() syntax)
     #by subtracting our bitwise value that comes in with teh data from the target bit, we check all the ands
     #so, if the target bit = 6, that is geolocation Qual and classification quality set to 1.
     #e.g.
     
        #  [1] "target bit= 507510784"
        # [1] "as vector"
        #  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1 1
        # [1] "flag 23 is set" "flag 26 is set" "flag 27 is set" "flag 28 is set"
        # [5] "flag 29 is set"

        # [1] "bitwise bit= 67108864"
        # [1] "as vector"
        #  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
        # [1] "flag 27 is set"
        # [1] " "
        # [1] "are any example flags in the target set?"
        # [1] TRUE

    target_vector= +(bitwAnd(target_bit_in, 2^seq(0,28))>0)
    target_flags=which(target_vector ==1)

    this_vector= (+(bitwAnd(bitwise_in, 2^seq(0,28))>0))
    this_flags=which(this_vector == 1)
     
     #    print(this_flags)
     # print(target_flags)

    bitfail= any(this_flags %in% target_flags)
     
     # print(bitfail)
     #return 0 if it fails, which means that the 'fail' condition is set to true
    if(bitfail == TRUE){return(0)}else{return(1)}
        }


    ################
    
  #make non binary flags binary
    #syntax below returns 1 when condition is met
    # e.g. +() syntax 
    ice_flag = +(ice_flag <= ice_max)
    dark_flag = +(dark_flag <= dark_max)
    xover_flag = +(xover_flag <= xover_cal_q_max)
    prior_width_flag = +(prior_width >= prior_width_min)

    #make a matrix of bitwise filtered data
    bitwise_flag = do.call(cbind,lapply(bitwise_flag,bitwiser,target_bit_in= target_bit_reach))
    
    #get the xtrack distance and filter for both min and max, add, and return anything =2
    #retun 1 for greater than the min
    xtrack_flag_min = +(abs(xtrack_flag) >= cross_track_dist_min_m)
    #retun 1 for less than than the max
    xtrack_flag_max = +(abs(xtrack_flag) <= cross_track_dist_max_m)
    #combine (slowly, but for repro)
    xtrack_combined= xtrack_flag_max + xtrack_flag_min
    #we want values ==2
    xtrack_flag= +( xtrack_combined ==2 ) 
    
    reach_length_flag = +(reach_length_flag >= reach_length_min_m)
    
    obs_frac_flag = +(obs_frac_flag > obs_frac_min)
    
    #deprecated in v 0002
    # wse_u_flag = +(wse_u_flag <= wse_r_u_max)
    # slope_u_flag = +(slope_u_flag <= slope_r_u_max)

 
  #na will mess it up later
  ice_flag[is.na(ice_flag)]=0
  dark_flag[is.na(dark_flag)]=0
  xover_flag[is.na(xover_flag)]=0
  prior_width_flag[is.na(prior_width_flag)]=0
  bitwise_flag[is.na(bitwise_flag)]=0
  xtrack_flag[is.na(xtrack_flag)]=0
  reach_length_flag[is.na(reach_length_flag)]=0
  obs_frac_flag[is.na(obs_frac_flag)]=0 
    
    #deprecated in v0002
  # slope_u_flag[is.na(slope_u_flag)]=0     
  # wse_u_flag[is.na(wse_u_flag)]=0 
        
    ice_flag=matrix(ice_flag,nrow=1,ncol=length(ice_flag))
    dark_flag=matrix(dark_flag,nrow=1,ncol=length(dark_flag))
    xover_flag=matrix(xover_flag,nrow=1,ncol=length(xover_flag))
    prior_width_flag=matrix(prior_width_flag,nrow=1,ncol=length(prior_width_flag))
    bitwise_flag=matrix(bitwise_flag,nrow=1,ncol=length(bitwise_flag))
    xtrack_flag=matrix(xtrack_flag,nrow=1,ncol=length(xtrack_flag))
    reach_length_flag=matrix(reach_length_flag,nrow=1,ncol=length(reach_length_flag))
    obs_frac_flag=matrix(obs_frac_flag,nrow=1,ncol=length(obs_frac_flag))
    
    #depreceated in v0002
    # wse_u_flag=matrix(wse_u_flag,nrow=1,ncol=length(wse_u_flag))
    # slope_u_flag=matrix(slope_u_flag,nrow=1,ncol=length(slope_u_flag))
  

  #we now have binary flags for 1 = KEEP, 0 = DROP
  #this is an 'or' flag, so we multiply
    
    #deprecated v0002
    # wse_u = wse_u_flag,
    # slope_u = slope_u_flag

  
master_flag=ice_flag*dark_flag*xover_flag*prior_width_flag*bitwise_flag*reach_length_flag*xtrack_flag*obs_frac_flag
    
    #deprecated v0002
    #*wse_u_flag*slope_u_flag
   
    ntot=sum(master_flag)
    

  
    Wobs=data$width
     # print('wse')
  Hobs=data$wse
     # print('slope')
  Sobs=data$slope
     # print('slope')
  S2obs=data$slope2
    
    # print('after flags defined')
    # print(data)
    
    # print('as first on first read into the flags function')
    #   print('slopes')
    # print(Sobs)
    # print('wse')
    # print(Hobs)
    # print('width')
    # print(Wobs)
    # print('slope2')
    # print(S2obs)
    # # bonk
    
#     print('master flag')
#     print(master_flag)
    
    
    
    # print('ice')
    # print(ice_flag)
    # print('dark')
    # print(dark_flag)#=matrix(dark_flag,nrow=1,ncol=length(dark_flag))
    # print('xover')
    # print(xover_flag)#=matrix(xover_flag,nrow=1,ncol=length(xover_flag))
    # print('p_width')
    # print(prior_width_flag)#=matrix(prior_width_flag,nrow=1,ncol=length(prior_width_flag))
    # print('bitwise')
    # print(bitwise_flag)#=matrix(bitwise_flag,nrow=1,ncol=length(bitwise_flag))
    # print('xtrack')
    # print(xtrack_flag)#=matrix(xtrack_flag,nrow=1,ncol=length(xtrack_flag))
    # print('prior length')
    # print(reach_length_flag)#=matrix(reach_length_flag,nrow=1,ncol=length(reach_length_flag))
    # print('obs frac')
    # print(obs_frac_flag)#=matrix(obs_frac_flag,nrow=1,ncol=length(obs_frac_flag))
  
  # Find anywhere where 0 and replace with NA
  Wobs[master_flag == 0]=NA
  Hobs[master_flag == 0]=NA
  Sobs[master_flag == 0]=NA
  S2obs[master_flag == 0]=NA
    
#       print('width after flagging NA assignment')
#     print(Wobs)
    
    
    # print(Hobs)
    # print(Sobs)
    # print(S2obs)
    # bonk
  
    #rewrite
  data$width=Wobs
  data$wse=Hobs
  data$slope=Sobs
  data$slope2=S2obs
    

    
#     print('NAs we inhereted from hydrochron upstream of this module')
#     print(hydrochronna)
    
#     print('slope NA in data object after filtering')
#     naindex=which(is.na(data$slope))
#     print(naindex)
    
#     print('flags ON')
#     master_index=which(master_flag==0)
#     print(master_index)
    
#     print('NAs unique to flags')
#     uniquetoflags=(setdiff(master_index,hydrochronna))
#     print(uniquetoflags)
    
#     print('does this filtered data object have an NA slope for every flag?')
#     print(all(master_index %in% naindex))
    
#     print('are the total flags exactly the union of the hydrochron NAs and the flagged NAs?')
#     totalexpected=union(hydrochronna,master_index)
#     print(setequal(totalexpected,naindex))
   
    
#     print('slope after reassigning filtered data to the data object passed to output')
#     print(data$slope)
    
#     print('after recombining')
#     print(data)

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
                                    obs_frac_flag=+(!obs_frac_flag),
                                    # slope_u_flag=+(!slope_u_flag),
                                    # wse_u_flag =+(!wse_u_flag),
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
                           prior_width_min,target_bit_node,cross_track_dist_min_m,
                          cross_track_dist_max_m, n_node_pix_min
                         ) {
  
    #dprectaed inputs v0002   slope_r_u_max,wse_r_u_max
    
  #read in flags
  ice_flag=data$ice_clim_f
  dark_flag=data$dark_frac
  xover_flag=data$xovr_cal_q
  prior_width=data$prior_width
  bitwise_flag=data$bitwise
  # reach_length_flag=data$reach_length
  xtrack_flag=data$cross_track_dist
  n_good_pix= data$n_good_pix 
    
    # print(n_good_pix)
  # Wobs=data$width
  # Hobs=data$wse
  # Sobs=data$slope
  # S2obs=data$slope2
    
    # print(Wobs)
    
    # print(xtrack_flag)
    
    
    #deprecated v0002
  # wse_u_flag=data$wse_r_u
  # slope_u_flag=data$slope_r_u
    
 
    
   
#convert bitwise integers to bit arrays
 bitwiser_node=function(bitwise_in, target_bit_in){
     #here, we do a bitwise and and turn it into a binary (that is the +() syntax)
     #by subtracting our bitwise value that comes in with teh data from the target bit, we check all the ands
     #so, if the target bit = 6, that is geolocation Qual and classification quality set to 1.
     #e.g.
     
        #  [1] "target bit= 507510784"
        # [1] "as vector"
        #  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 1 1 1 1
        # [1] "flag 23 is set" "flag 26 is set" "flag 27 is set" "flag 28 is set"
        # [5] "flag 29 is set"

        # [1] "bitwise bit= 67108864"
        # [1] "as vector"
        #  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0
        # [1] "flag 27 is set"
        # [1] " "
        # [1] "are any example flags in the target set?"
        # [1] TRUE

    target_vector= +(bitwAnd(target_bit_in, 2^seq(0,28))>0)
    target_flags=which(target_vector ==1)

    this_vector= (+(bitwAnd(bitwise_in, 2^seq(0,28))>0))
    this_flags=which(this_vector == 1)
     
  

    bitfail= any(this_flags %in% target_flags)
     

     #return 0 if it fails, which means that the 'fail' condition is set to true 
    if(bitfail == TRUE){return(0)}else{return(1)}
        }
        

     
  #make non binary flags binary
    #syntax below returns 1 when condition is met
    ice_flag = +(ice_flag <= ice_max)
    dark_flag = +(dark_flag <= dark_max)
    xover_flag = +(xover_flag <= xover_cal_q_max)
    prior_width_flag = +(prior_width >= prior_width_min)
    #in node mode, we need to apply bitwiser to all elements of the matrix
    bitwise_flag_node=matrix(nrow=nrow(bitwise_flag),ncol=ncol(bitwise_flag))
    good_pix_flag= +(n_good_pix >= n_node_pix_min)
    
    for (i in 1:nrow(bitwise_flag)){
        for(j in 1:ncol(bitwise_flag)){
            this_bit= bitwiser_node(bitwise_flag[i,j],target_bit_node)
             bitwise_flag_node[i,j]=this_bit
            }
        }

    bitwise_flag = bitwise_flag_node
    
    #get the xtrack distance and filter for both min and max, add, and return anything =2
    #retun 1 for greater than the min
    
    #swath values can go negative, so use abs xtrack distance
    # print(xtrack_flag)
    xtrack_flag_min = +(abs(xtrack_flag) >= cross_track_dist_min_m)
    
    #retun 1 for less than than the max
    xtrack_flag_max = +(abs(xtrack_flag) <= cross_track_dist_max_m)
    
    #combine (slowly, but for repro)
    xtrack_combined= xtrack_flag_max + xtrack_flag_min
    
    # print(xtrack_combined)
    #we want values ==2
    xtrack_flag= +( xtrack_combined ==2 ) 
    
    # print(xtrack_flag)
    
    # deprecated v0002
    # wse_u_flag = +(wse_u_flag <= wse_r_u_max)
    # slope_u_flag = +(slope_u_flag <= slope_r_u_max)
     
    
 
  #na will mess it up later
  ice_flag[is.na(ice_flag)]=0
  dark_flag[is.na(dark_flag)]=0
  xover_flag[is.na(xover_flag)]=0
  prior_width_flag[is.na(prior_width_flag)]=0
  bitwise_flag[is.na(bitwise_flag)]=0
  xtrack_flag[is.na(xtrack_flag)]=0
  good_pix_flag[is.na(good_pix_flag)]=0
    
  #deprecated v0002
  # slope_u_flag=good_pix_flag     ##dummy as of v2
  # wse_u_flag=good_pix_flag  ##dummy as of v2

#we now have binary flags for 1 = KEEP, 0 = DROP
#this is an 'or' flag, so we multiply

  master_flag=ice_flag*dark_flag*xover_flag*prior_width_flag*bitwise_flag*xtrack_flag*good_pix_flag
    
    #deprecated v0002
    #*wse_u_flag*slope_u_flag
  

  Wobs=data$width
  Hobs=data$wse
  Sobs=data$slope
  S2obs=data$slope2
    

    # print('slope on first read into the flags function')
    # print(Sobs)
    # print(Hobs)
    # print(Sobs)
    # print(S2obs)

    
    # print('master flag')
    # print(master_flag)
  
  # Find anywhere where 0 and replace with NA
  Wobs[master_flag == 0]=NA
  Hobs[master_flag == 0]=NA
  Sobs[master_flag == 0]=NA
  S2obs[master_flag == 0]=NA
    
#         print('after NA assignments')
#     print(Sobs)
#     print(Hobs)
#     print(Sobs)
#     print(S2obs)
    
      

    
#     print('ice')
#     print(sum(ice_flag==0))
    
#     print('dark')
#     print(sum(dark_flag==0))#=matrix(dark_flag,nrow=1,ncol=length(dark_flag))
    
#     print('xover')
#     print(sum(xover_flag==0))#=matrix(xover_flag,nrow=1,ncol=length(xover_flag))
    
#     print('p_width')
#     print(sum(prior_width_flag==0))#=matrix(prior_width_flag,nrow=1,ncol=length(prior_width_flag))
    
#     print('bitwise')
#     print(sum(bitwise_flag==0))#=matrix(bitwise_flag,nrow=1,ncol=length(bitwise_flag))
    
#     print('xtrack')
#     print(sum(xtrack_flag==0))#=matrix(xtrack_flag,nrow=1,ncol=length(xtrack_flag))

#     print('good pix')
#     print(sum(good_pix_flag==0))#=matrix(obs_frac_flag,nrow=1,ncol=length(obs_frac_flag))
    
#     print('total flagged')
#     print(sum(master_flag==0))
    
#     print('total nt')
#     print(nrow(ice_flag)*ncol(ice_flag))
    
    
    # bonk

  
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
                                    dark_flag=+(!dark_flag), 
                                     xover_flag=+(!xover_flag),
                                    prior_width_flag=+(!prior_width_flag), 
                                     bitwise_flag=+(!bitwise_flag),
                                    xtrack_flag=+(!xtrack_flag),                                    
                                    good_pix_flag=+(!good_pix_flag))
                                    # slope_u_flag=+(!slope_u_flag), ##dummy as of v2
                                    # wse_u_flag =+(!wse_u_flag)) ##dummy as of v2
             ))
  
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
    
    # print('slope on first read from /mnt')
    # print(data$node_list$wse)
    # # print(data$reach_list$wse)
    # # print(data$reach_list$slope)
    # # print(data$reach_list$slope2)
    # bonk
    


  # Check if data is valid
    #this changes dimensions of data!!!!
  width <- data$reach_list$width[!is.na(data$reach_list$width)]
  wse <- data$reach_list$wse[!is.na(data$reach_list$wse)]
  slope <- data$reach_list$slope[!is.na(data$reach_list$slope)]
    

    #this doesn't guarantee these are the sazme!!!
  if (length(width) > 1 || length(wse) > 1 || length(slope) > 1) {
    
    # Apply flags to reach and node data
    
    reach_diag_data <- apply_flags_reach(data$reach_list,
                                         ice_max=GLOBAL_PARAMS$ice_max, 
                                         dark_max=GLOBAL_PARAMS$dark_max, 
                                         xover_cal_q_max=GLOBAL_PARAMS$xover_cal_q_max, 
                                         prior_width_min=GLOBAL_PARAMS$prior_width_min, 
                                         target_bit_reach=GLOBAL_PARAMS$target_bit_reach, 
                                         cross_track_dist_min_m=GLOBAL_PARAMS$cross_track_dist_min_m, 
                                         cross_track_dist_max_m=GLOBAL_PARAMS$cross_track_dist_max_m,
                                         reach_length_min_m=GLOBAL_PARAMS$reach_length_min_m,
                                         # wse_r_u_max=GLOBAL_PARAMS$wse_r_u_max,
                                         # slope_r_u_max=GLOBAL_PARAMS$slope_r_u_max,
                                         obs_frac_min=GLOBAL_PARAMS$obs_frac_min)
      
      #all NA here
      # print('after apply_flags_reach')
      # print(reach_diag_data$data$slope)
      # bonk
    
    reach_list <- reach_diag_data$data
    reach_flags <- reach_diag_data$flags
    node_diag_data <- apply_flags_node(data$node_list, 
                                         ice_max=GLOBAL_PARAMS$ice_max, 
                                         dark_max=GLOBAL_PARAMS$dark_max, 
                                         xover_cal_q_max=GLOBAL_PARAMS$xover_cal_q_max, 
                                         prior_width_min=GLOBAL_PARAMS$prior_width_min, 
                                         target_bit_node=GLOBAL_PARAMS$target_bit_node, 
                                         cross_track_dist_min_m=GLOBAL_PARAMS$cross_track_dist_min_m,
                                         cross_track_dist_max_m=GLOBAL_PARAMS$cross_track_dist_max_m,
                                         n_node_pix_min = GLOBAL_PARAMS$n_node_pix_min)
                                         # wse_r_u_max=GLOBAL_PARAMS$wse_r_u_max,
                                         # slope_r_u_max=GLOBAL_PARAMS$slope_r_u_max)
    node_list <- node_diag_data$data
    node_flags <- node_diag_data$flags

    # print('after apply flags node')
    #   print(head(node_list))

    
    # Apply sesame street filter to reach and node data
    reach_ses_diags <- sesame_street(reach_list, GLOBAL_PARAMS$Tukey_number)
    reach_list <- reach_ses_diags$data
    reach_outliers <- reach_ses_diags$flags
     
      # print('additional reach outlier flags')
      # print(reach_outliers)
      
    node_ses_diags <- sesame_street(node_list, GLOBAL_PARAMS$Tukey_number)
    node_list <- node_ses_diags$data
    node_outliers <- node_ses_diags$flags
      
      #  print('after sesame  node')
      # print(head(node_list))

      #' Apply filter to filter out low slope
#'
#' @param data dataframe
#' @param min_slope 
#'
#' @return dataframe
low_slope=function(data, sword_slope, min_slope, level){
  
# if(!nrow(data$width>1)){
# print('read into low slope diags')
#     print(data$slope)}

###MIXED SLOPE TOGGLE 1/2/25---------------------

    #we want to use the sword slope, unless it is less than the slope minimum
    #check sword against the slope min, and then assign whichever is higher to 
    #swot data that are < the slope minimum.
# if (sword_slope > min_slope) {
#     slope_value <- sword_slope
#   } else {
#     slope_value <- min_slope
# }

#     #reset any slope less than the minimum slope value to the slope value
#     data$slope[data$slope< min_slope]= slope_value
#     data$slope2[data$slope2< min_slope]= slope_value
#     # #return a '1' where we have masked to slope
#     # #the +() syntax will binarize
#     slope_flags=+(data$slope< min_slope)

    # print(slope_flags)
### mixed slope toggle end----------------------
   
 
### FIXED SLOPE TOGGLE 1/2/25  ------------------
  #in this case, if we have low slopes, we want to set all slopes to a constant value equal to the 
    #sword slope, which approximates the bed slope. This prevents behavior where HWS appaear to vary
    #out of step one another
    

# if (sword_slope > min_slope) {
    slope_value = sword_slope
#   } else {
#     slope_value = min_slope
# }
    
    #dont write if the input is all NA, this will be confusing later
    if(all(is.na(data$slope))){
        # print('allna')
            length = dim(data$slope)
          if (level == "reach") {
      
            slope_flags <- rep(1, times=length)
          } else {
         
            slope_flags <- array(1, dim=length)
          }
      
       
        return(list(data=data, flags=slope_flags))}
    
  if (any(data$slope< slope_value,na.rm=TRUE)){
      
        # print(paste('low slope on',level))
      # print(slope_value)

      # Set slope and slope2
      length = dim(data$slope)
   
          if (level == "reach") {
            data$slope <-  rep(slope_value, times=length)
            data$slope2 <- rep(slope_value, times=length)
            slope_flags <- rep(1, times=length)
          } else {
            data$slope <-  array(slope_value, dim=length)
            data$slope2 <- array(slope_value, dim=length)
            slope_flags <- array(1, dim=length)
          }
      
            
#       print(data$slope)
#       bonk
      
      } else {
         length = dim(data$slope)
        if (level == "reach") {
            slope_flags <- rep(0, times=length)
            } else {
            slope_flags <- array(0, dim=length)
        }  

      
   }
    ### fixed slope toggle end------------------
    # if(!nrow(data$width>1)){
    # print('coming out of low slope diags')
    # print(data$slope)}

      return(list(data=data, flags=slope_flags))

 } #end low slope function

    # Apply low slope filter to reach and node data

      reach_low_slope_diags <- low_slope(data=reach_list, sword_slope=data$sword_slope,
                                         min_slope=GLOBAL_PARAMS$prior_slope_min, level="reach")
      reach_list <- reach_low_slope_diags$data
      
      # print('after low slope diags')
      # print(reach_list$slope)
      # bonk
      
      reach_slope_flags <- reach_low_slope_diags$flags
      
      node_low_slope_diags <- low_slope(data=node_list, sword_slope=data$sword_slope,
                                        min_slope=GLOBAL_PARAMS$prior_slope_min, level="node")
      node_list <- node_low_slope_diags$data
      node_slope_flags <- node_low_slope_diags$flags

    # Filter d_x_area based on width and wse
    reach_dxa_diags <- filter_dxa(reach_list, "reach")
    reach_list <- reach_dxa_diags$data
    reach_dxa_flags <- reach_dxa_diags$flags
    node_dxa_diags <- filter_dxa(node_list, "node")
    node_list <- node_dxa_diags$data
    node_dxa_flags <- node_dxa_diags$flags
    
#            ## debugging toggle-------------
   
#       outmat=data.frame(
#           'ice'=sum(reach_flags$ice_flag)/length(reach_flags$ice_flag),
#           'p_width'=sum(reach_flags$prior_width_flag)/length(reach_flags$ice_flag),
#           'dark'=sum(reach_flags$dark_flag)/length(reach_flags$ice_flag),
#           'p_length'=sum(reach_flags$reach_length_flag)/length(reach_flags$ice_flag),
#           'xtrack'=sum(reach_flags$xtrack_flag)/length(reach_flags$ice_flag),
#           'bitwise'=sum(reach_flags$bitwise_flag)/length(reach_flags$ice_flag),
#           'xover'=sum(reach_flags$xover_flag)/length(reach_flags$ice_flag),
#           'obs_frac'=sum(reach_flags$obs_frac_flag)/length(reach_flags$ice_flag),
#            'n_good_pix_node'=sum(node_flags$good_pix_flag)/length(node_flags$ice_flag),
#           # 'slope_u'=sum(reach_flags$slope_u_flag)/length(reach_flags$ice_flag),
#           # 'wse_u'=sum(reach_flags$wse_u_flag)/length(reach_flags$ice_flag),
#           'master'=sum(reach_flags$master_flag)/length(reach_flags$ice_flag),
#           'ntot'=reach_flags$ntot,
#           'reach_id'=reach_files$reach_id)
                    

#     return( list(reach_flags=reach_flags, 
#                  node_flags=node_flags, 
#                  reach_outliers=reach_outliers, 
#                  node_outliers=node_outliers,
#                  reach_slope_flags=reach_slope_flags, 
#                  node_slope_flags=node_slope_flags,
#                  reach_dxa_flags=reach_dxa_flags,
#                  node_dxa_flags=node_dxa_flags))
      
#        ##end debugging toggle---------------
      
    
#       print('reach')
#     print(reach_list$slope)
#       print('node')
#       print(node_list$slope)
      
#       bonk
      
#output of diagnostics
    write_data(reach_list, node_list, reach_flags, node_flags, reach_outliers, 
               node_outliers, reach_slope_flags, node_slope_flags, reach_dxa_flags,
               node_dxa_flags, reach_files$swot, output_dir, GLOBAL_PARAMS, data)
    message(paste0(reach_files$reach_id, ": Node and reach files overwritten."))
  
    } else {
    message(paste0(reach_files$reach_id, ": Invalid data; node and reach files not modified."))
  }
} #end run diagnostics