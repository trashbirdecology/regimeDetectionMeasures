#' @title Calculate the early warning signals
#' @param winMove Proportion of data to be included in each moving window (0,1).
#' @param distances A data frame of the distances and dervatives of distance travelled at each time point.
#' @param dataIn Used in calc_FisherInformation. Default = 2 data points
#' @export
calculate_EWS <- function(dataIn, winMove){

    # Create function for getting mode of data
    getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
    }



    require(PerformanceAnalytics)


    # List of variables with at least one observation.
    spp <- dataIn %>%
        group_by(variable) %>%
        summarise(sum=sum(value)) %>%
        filter(sum > 0) %>%
        select(variable)


    ews <- dataIn %>%
        filter(variable %in% spp$variable) %>%
        group_by(variable) %>%
        arrange(variable, time) %>%
        mutate(
            mean = mean(value),
            mode = getmode(value),
            sd = sd(value),
            CV = sd(value)/mean(value),
            # kurtosis using the "PerformanceAnalytics" function
            kurtosis =  kurtosis(value, method = "fisher"),
            # skewness using two methods
            skewMode = (mean(value)-getmode(value))/sd(value),
            # autocorr1 = acf(value, lag = 1)[1]$acf %>% as.numeric(),
            skewMean = (median(value)*3)/sd(value)
        ) %>%
        ungroup() %>%
        select(-time, -value) %>%
        distinct() %>%
        na.omit(mean) %>%
        mutate(winStart = min(dataIn$time)) %>%
        mutate(winStop = max(dataIn$time))


    return(ews)

}
