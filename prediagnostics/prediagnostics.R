

#' Run diagnostics on SWOT data
#'
#' @param input_dir string path to input directory
#' @param reaches_json string name of JSON reach file
#' @param index integer used to index reaches_json file
#' @param output_dir string path to output directory
run_diagnostics <- function(input_dir, reaches_json, index, output_dir) {
    
#     #debuggng
# source("/nas/cee-water/cjgleason/colin/prediagnostics/prediagnostics/input.R")
# source("/nas/cee-water/cjgleason/colin/prediagnostics/prediagnostics/output.R")
# source("/nas/cee-water/cjgleason/colin/prediagnostics/prediagnostics/prediagnostics.R")
# source("/nas/cee-water/cjgleason/colin/prediagnostics/prediagnostics/outlier_filter.R")
# source("/nas/cee-water/cjgleason/colin/prediagnostics/prediagnostics/apply_flags_reach.R")
# source("/nas/cee-water/cjgleason/colin/prediagnostics/prediagnostics/apply_flags_node.R")
# source("/nas/cee-water/cjgleason/colin/prediagnostics/prediagnostics/filter_dxa.R")
# source("/nas/cee-water/cjgleason/colin/prediagnostics/prediagnostics/config.R")

    
    library(dplyr)

  
  # Retrieve input data
  reach_files <- get_reach_files(reaches_json, input_dir, index)
    

  data <- get_data(reach_files)


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
                                         prior_reach_width_min_m=GLOBAL_PARAMS$prior_reach_width_min_m, 
                                         target_bit_reach=GLOBAL_PARAMS$target_bit_reach, 
                                         cross_track_dist_min_m=GLOBAL_PARAMS$cross_track_dist_min_m, 
                                         cross_track_dist_max_m=GLOBAL_PARAMS$cross_track_dist_max_m,
                                         reach_length_min_m=GLOBAL_PARAMS$reach_length_min_m,
                                         obs_frac_min=GLOBAL_PARAMS$obs_frac_min)
      
    
    reach_list <- reach_diag_data$data
    reach_flags <- reach_diag_data$flags
    node_diag_data <- apply_flags_node(data$node_list, 
                                         ice_max=GLOBAL_PARAMS$ice_max, 
                                         dark_max=GLOBAL_PARAMS$dark_max, 
                                         xover_cal_q_max=GLOBAL_PARAMS$xover_cal_q_max, 
                                         prior_node_width_min_m=GLOBAL_PARAMS$prior_node_width_min_m, 
                                         target_bit_node=GLOBAL_PARAMS$target_bit_node, 
                                         cross_track_dist_min_m=GLOBAL_PARAMS$cross_track_dist_min_m,
                                         cross_track_dist_max_m=GLOBAL_PARAMS$cross_track_dist_max_m,
                                         n_node_pix_min = GLOBAL_PARAMS$n_node_pix_min)

    node_list <- node_diag_data$data
    node_flags <- node_diag_data$flags

    
    # Apply sesame street filter to reach and node data
    reach_ses_diags <- outlier_filter(reach_list, GLOBAL_PARAMS$Tukey_number)
    reach_list <- reach_ses_diags$data
    reach_outliers <- reach_ses_diags$flags
     
    node_ses_diags <- outlier_filter(node_list, GLOBAL_PARAMS$Tukey_number)
    node_list <- node_ses_diags$data
    node_outliers <- node_ses_diags$flags
      

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
#                  reach_dxa_flags=reach_dxa_flags,
#                  node_dxa_flags=node_dxa_flags))
      
#        ##end debugging toggle---------------
      
    

      
#output of diagnostics
    write_data(reach_list, node_list, reach_flags, node_flags, reach_outliers, 
               node_outliers, reach_dxa_flags,
               node_dxa_flags, reach_files$swot, output_dir, GLOBAL_PARAMS, data)
    message(paste0(reach_files$reach_id, ": Node and reach files overwritten."))
  
    } else {
    message(paste0(reach_files$reach_id, ": Invalid data; node and reach files not modified."))
  }
} #end run diagnostics