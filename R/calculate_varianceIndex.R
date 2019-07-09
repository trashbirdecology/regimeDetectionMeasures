#' @title Calculate the Variance Index
#' @description The Variance Index is the maximum eigenvalue of the variance covariance matrix, and is described in Brock et al. (2006).
#' @param winData A data frame.
#' @param fill Fill for missing data. Default = 0.
#' @references Brock, William A., and Stephen R. Carpenter. "Variance as a leading indicator of regime shift in ecosystem services." Ecology and Society 11.2 (2006).

calculate_VI <- function(winData, fill = 0) {

  # Remove site variable if present
  if("site" %in% colnames(winData)){
    winData <- winData %>% dplyr::select(-site)
      }
  # Ensure fill works (having troubles..)
  if(!exists("fill")){
  fill = 0
}


 # Create the time series to analyse
   ts <-
    winData %>%
    distinct() %>%
    group_by(variable) %>%
    mutate(sumValue = sum(value)) %>%
    filter(sumValue > 0) %>%
    dplyr::select(-sumValue) %>%
    spread(variable, value, fill = fill) %>%
    dplyr::select(-sortVar, -cellID) %>%
    as.matrix()


    eigCov <- eigen(cov(ts))
    VI <- max(eigCov$values)

    if(is.null(VI)){ VI = NA  }

    VI <- data.frame(VI, min(winData$cellID), max(winData$cellID))
    names(VI) <- c("metricValue", "cellID_min", 'cellID_max')

    return(VI)


}

