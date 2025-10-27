

# October 13 2025 v "Permissive relax" params
GLOBAL_PARAMS = list (
     prior_reach_width_min_m = 60 ,
     prior_node_width_min_m = 70 ,
     prior_slope_min = 3.4e-5,
     reach_length_min_m=5000,
     cross_track_dist_min_m= 10000,
     cross_track_dist_max_m= 60000,
     ice_max = 1,
     target_bit_reach= 507510784, #FUNCTIONALLY EQUIVALENT TO _q<=2
     target_bit_node=  532680192 , #adds flags 9,10, 11, 23, and 24 to the reach
     dark_max = 0.6,
     obs_frac_min= 0.4,
     xover_cal_q_max = 1,
     Tukey_number = 1.5,
     n_node_pix_min =10
)

# # Jan 13 2025 v "Montpellier" params
# GLOBAL_PARAMS = list (
#      prior_width_min_m = 80 ,
#      prior_slope_min = 3.4e-5,
#      reach_length_min_m=7000,
#      cross_track_dist_min_m= 10000,
#      cross_track_dist_max_m= 60000,
#      ice_max = 0,
#      target_bit_reach= 507510784, #FUNCTIONALLY EQUIVALENT TO _q<=2
#      target_bit_node=  532680192 , #adds flags 9,10, 11, 23, and 24 to the reach
#      dark_max = 0.4,
#      obs_frac_min= 0.5,
#      xover_cal_q_max = 1,
#      Tukey_number = 1.5,
#      n_node_pix_min =10
# )
