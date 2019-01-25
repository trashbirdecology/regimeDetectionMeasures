#' Calculate the `distance travelled` by the entire system.
#'
#' Also calculates the velocity and acceleration of the entire system over the time series.
#'
#' @param dataIn input dataframe
#' @param derivs logical (default TRUE). calculate derivatives as well
#' @param print logical (default TRUE). print output
#'
#' @export
#'
calculate_distanceTravelled <-
    function(dataInDist,
             derivs = T,
             print = T) {

        distances <- dataInDist %>% group_by(variable) %>%
            arrange(variable, sortVar) %>%
            mutate(dx = value - lag(value)) %>%
            na.omit(dx) %>%
            ungroup() %>%
            group_by(sortVar) %>%
            mutate(ds = sqrt(sum(dx ^ 2))) %>%
            ungroup() %>%
            distinct(sortVar,  ds, .keep_all = T) %>%
            dplyr::select(cellID, sortVar, ds) %>%
            arrange(sortVar) %>%
            mutate(s = cumsum(ds)) %>%
            ungroup()

        if (derivs == T) {
            distances <- distances %>% mutate(dsdt = ((s - lag(s)) / (sortVar -
                                                                          lag(sortVar))), d2sdt2 = ((dsdt - lag(dsdt)) /
                                                                                                        (sortVar - lag(sortVar)))) %>% ungroup()
        }
        if (print == T) {
            head(distances)
        }

        distances <-
            as.data.frame(distances) # being buggy with group by and gather. keep for now.

        return(distances)

    }
