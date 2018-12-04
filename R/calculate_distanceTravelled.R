#' @title Calculate the `distance travelled` by the entire system. Also calculates the velocity and acceleration of the entire system over the time series.
#'
#' @param dataIn
#' @param derivs
#' @param print
#'
#' @export
#'
calculate_distanceTravelled <- function(dataIn, derivs = T, print = T) {
    distances <- dataIn %>%
        # Need to group by variable
        group_by(variable) %>%
        # Calculate the Euclidean distance for each variable between two time points.
        arrange(variable, time) %>%
        # Calculates first difference
        mutate(dx = value - lag(value)) %>%
        # Remove the first dx for each variable because it == NA
        na.omit(dx) %>%
        # Remove the grouping factor
        ungroup() %>%
        group_by(time) %>%
        # Sum the dx across all species at each time point
        summarize(ds = sqrt(sum(dx ^ 2))) %>%
        # Calculate cumulative sum of ds
        mutate(s = cumsum(ds))

    if (derivs == T) {
        distances <- distances %>%
            mutate(dsdt = ((s - lag(s)) / (time - lag(time))),
                   d2sdt2 = ((dsdt - lag(dsdt)) / (time - lag(time)))) %>%
            ungroup()

    } # Calculate the derivatives of the cumulative sum (s)

    if(print == T){head(distances)} # Print df to screen

    return(distances)

}
