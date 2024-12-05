# Dec 4 2024 v 0002 params
GLOBAL_PARAMS = list (
     prior_width_min_m = 80 ,
     prior_slope_min = 3.4e-5,
     reach_length_min_m=7000,
     cross_track_dist_min_m= 10000,
     cross_track_dist_max_m= 60000,
     ice_max = 0,
     target_bit_reach= 507510784, #FUNCTIONALLY EQUIVALENT TO _q<=2
     target_bit_node=  532680192 , #adds flags 9,10, 11, 23, and 24 to the reach
     dark_max = 0.4,
     obs_frac_min= 0.5,
     xover_cal_q_max = 1,
     Tukey_number = 1.5,
     n_node_pix_min =10
     # slope_r_u_max=10e-5 ## deprecated in v002
     # wse_r_u_max = 0.5 ## deprecated in v002
)

#v 0001 params, pre December 2024
#GLOBAL_PARAMS = list (
# wse_r_u_max = 0.5,
# dark_max = 0.1,
# slope_min = 1.7e-5,
# Tukey_number = 1.5,
# ice_max = 0,
# xover_cal_q_max = 1,
# prior_width_min = 80 ,
# target_bit= 168298510,
# cross_track_dist_min_m= 15000,
# reach_length_min_m=7000,
# slope_r_u_max=10e-5  
#)
