#seasame street filter
seasame_street=function(data,Tukey_number){
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

#flag pass
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
  
  return(data)
  
}

