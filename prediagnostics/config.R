# # jan 14 2025 "no filter" params
# GLOBAL_PARAMS = list (
#      prior_reach_width_min_m = 1 ,
#      prior_node_width_min_m = 1 ,
#      prior_slope_min = 3.4e-50,
#      reach_length_min_m=70,
#      cross_track_dist_reach_min_m= 0,
#      cross_track_dist_node_min_m= 0,
#      cross_track_dist_reach_max_m= 60000000,
#      cross_track_dist_node_max_m= 60000000,
#      ice_max = 10,
#      target_bit_reach= 32, #does not exist, so won't trip
#      target_bit_node=  32 , #does not exist, so won't trip
#      dark_max = 0.99,
#      obs_frac_min= 0.01,
#      xover_cal_q_max = 10,
#      Tukey_number = 50,
#      n_node_pix_min =1
#      # slope_r_u_max=10e-5 ## deprecated in v002
#      # wse_r_u_max = 0.5 ## deprecated in v002
# )

# Jan 13 2025 v "permissive" params
GLOBAL_PARAMS = list (
     prior_reach_width_min_m = 80 ,
     prior_node_width_min_m = 80 ,
     prior_slope_min = 3.4e-5,
     reach_length_min_m=7000,
     cross_track_dist_reach_min_m= 10000,
     cross_track_dist_node_min_m= 10000,
     cross_track_dist_reach_max_m= 60000,
     cross_track_dist_node_max_m= 60000,
     ice_max = 0,
     target_bit_reach= 507510784, #FUNCTIONALLY EQUIVALENT TO _q<=2
     target_bit_node=  532680192 , #adds flags 9,10, 11, 23, and 24 to the reach
     dark_max = 0.4,
     obs_frac_min= 0.5,
     xover_cal_q_max = 1,
     Tukey_number = 1.5,
     n_node_pix_min =10,
     # 0: main branche, replace dynamic slope by static slope if 1 obs < statics slope
     # 1: keep dynamic slope
     # 2: replace dynamic slope by static slope
     slope_option=0
     # slope_r_u_max=10e-5 ## deprecated in v002
     # wse_r_u_max = 0.5 ## deprecated in v002
)

# # Jan 17 2025 v "strict" params
# GLOBAL_PARAMS = list (
#      prior_reach_width_min_m = 80 ,#same as Kostas
#      prior_node_width_min_m = 80 ,#same as Kostas
#      prior_slope_min = 1.7e-5,#same as Kostas
#      reach_length_min_m=7000,#same as Kostas
#      cross_track_dist_reach_min_m= 15000,#same as Kostas
#      cross_track_dist_node_min_m= 15000,#same as Kostas
#      cross_track_dist_reach_max_m= 600000, #setting to high number to reflect no max in Kostas run
#      cross_track_dist_node_max_m= 600000, #setting to high number to reflect no max in Kostas run
#      ice_max = 0,#same as Kostas
#      target_bit_reach= 168298510, #same as Kostas
#      target_bit_node=  168298510, #same as Kostas
#      dark_max = 0.1,#same as Kostas
#      obs_frac_min= 0.01, #setting super low to recreate Kostas run, which didn't have this filter
#      xover_cal_q_max = 1,#same as Kostas
#      Tukey_number = 1.5,#same as Kostas
#      n_node_pix_min =1 #setting super low to recreate Kostas run, which didn't have this filter
#      # slope_r_u_max=10e-5 ## deprecated 
#      # wse_r_u_max = 0.5 ## deprecated 
# )

#v 0001 params, pre December 2024
#GLOBAL_PARAMS = list (
# wse_r_u_max = 0.5,
# dark_max = 0.1,
# slope_min = 1.7e-5,
# Tukey_number = 1.5,
# ice_max = 0,
# xover_cal_q_max = 1,
# prior_reach_width_min_m = 80 ,
# prior_node_width_min_m = 80 ,
# target_bit= 168298510,
# cross_track_dist_reach_min_m= 15000,
# cross_track_dist_node_min_m= 15000,
# reach_length_min_m=7000,
# slope_r_u_max=10e-5  
#)
