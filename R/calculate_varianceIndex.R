#' @title Calculate the Variance Index
#' @param winData
#' @param dataIn
#' @param fill Fill for missing data (default = 0)
#' @references [1] Brock and Carpenter YYYY
#' @export
calculate_VI <- function(winData, fill = 0) {

    ts <-
        winData %>%
        select(-site) %>%
        # gather(variable, value, -time) %>%
        group_by(variable) %>%
        mutate(sumValue = sum(value)) %>%
        filter(sumValue > 0) %>%
        select(-sumValue) %>%
        # unique() %>%
        spread(variable, value, fill = fill) %>%
        select(-time) %>%
        as.matrix()


    eigCov <- eigen(cov(ts))
    VI <- max(eigCov$values)

    return(VI)


}

