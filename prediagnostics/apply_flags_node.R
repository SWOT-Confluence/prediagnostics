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
    xtrack_flag_min = +(abs(xtrack_flag) >= cross_track_dist_min_m)
    
    #retun 1 for less than than the max
    xtrack_flag_max = +(abs(xtrack_flag) <= cross_track_dist_max_m)
    
    #combine (slowly, but for repro)
    xtrack_combined= xtrack_flag_max + xtrack_flag_min

    #we want values ==2
    xtrack_flag= +( xtrack_combined ==2 ) 

 
  #na will mess it up later
  ice_flag[is.na(ice_flag)]=0
  dark_flag[is.na(dark_flag)]=0
  xover_flag[is.na(xover_flag)]=0
  prior_width_flag[is.na(prior_width_flag)]=0
  bitwise_flag[is.na(bitwise_flag)]=0
  xtrack_flag[is.na(xtrack_flag)]=0
  good_pix_flag[is.na(good_pix_flag)]=0


#we now have binary flags for 1 = KEEP, 0 = DROP
#this is an 'or' flag, so we multiply

  master_flag=ice_flag*dark_flag*xover_flag*prior_width_flag*bitwise_flag*xtrack_flag*good_pix_flag

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