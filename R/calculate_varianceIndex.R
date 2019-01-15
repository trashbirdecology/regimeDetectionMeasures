#' @title Calculate the Variance Index
#' @param winData
#' @param dataIn
#' @param fill Fill for missing data (default = 0)
#' @references [1] Brock and Carpenter YYYY
#' @export
calculate_VI <- function(winData, fill = 0) {
 
  # Remove site variable if present
  if("site" %in% colnames(winData)){
    winData <- winData %>% dplyr::select(-site)
      }
# Create the time series to analyse    
   ts <-
    winData %>%
    distinct() %>% 
    group_by(variable) %>%
    mutate(sumValue = sum(value)) %>%
    filter(sumValue > 0) %>%
    select(-sumValue) %>%
    group_by(variable, time) %>% 
    summarise(value = max(value)) %>%
    ungroup() %>% 
    spread(variable, value, fill = fill) %>%
    select(-time) %>%
    as.matrix()


    eigCov <- eigen(cov(ts))
    VI <- max(eigCov$values)

    return(VI)


}

