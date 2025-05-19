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