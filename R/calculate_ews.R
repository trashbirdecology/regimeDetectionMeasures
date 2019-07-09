#' @title Calculate the early warning signals
#' @description Outputs data frame 'ews'.
#' @export
#' @param winMove Proportion of data to be included in each moving window (0,1).
#' @param distances A data frame of the distances and dervatives of distance travelled at each time point.
#' @param winData Used in calc_FisherInformation. Default = 2 data points

calculate_EWS <- function(winData, winMove){
    # Create function for getting mode of data
    getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
    }

    require(PerformanceAnalytics)


    # List of variables with at least one observation.
    spp <- winData %>%
        dplyr::group_by(variable) %>%
        dplyr::summarise(sum = sum(value)) %>%
        filter(sum > 0) %>%
        dplyr::select(variable)


    ews <- winData %>%
        filter(variable %in% spp$variable) %>%
        group_by(variable) %>%
        arrange(sortVar) %>%
        mutate(
            mean = mean(value),
            mode = getmode(value),
            sd = sd(value),
            CV = sd(value) / mean(value),
            # kurtosis using the "PerformanceAnalytics" function
            kurtosis =  kurtosis(value, method = "fisher"),
            # skewness using two methods
            skewMode = (mean(value) - getmode(value)) / sd(value),
            # autocorr1 = acf(value, lag = 1)[1]$acf %>% as.numeric(),
            skewMean = (median(value) * 3) / sd(value)
        ) %>%
        ungroup() %>%
        dplyr::select(-sortVar,-value) %>%
        distinct() %>%
        na.omit(mean) %>%
        mutate(winStart = min(winData$sortVar)) %>%
        mutate(winStop = max(winData$sortVar)) %>%
        tidyr::gather(key = "metricType",
                      value = "metricValue",
                      -variable,-winStart,-winStop,-cellID) %>%
        mutate(cellID_min = min(winData$cellID)) %>%
        mutate(cellID_max = max(winData$cellID))


    return(ews)

}
