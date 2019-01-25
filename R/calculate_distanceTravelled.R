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

        distances <- dataInDist %>% group_by(variable) %>% arrange(variable,
                                                                   time) %>% mutate(dx = value - lag(value)) %>% na.omit(dx) %>%
            ungroup() %>%
            group_by(time) %>%
            mutate(ds = sqrt(sum(dx ^ 2))) %>%
            ungroup() %>%
            distinct(time,  ds,.keep_all=T) %>%
            dplyr::select(cellID, time, ds) %>%
            arrange(time) %>%
            mutate(s = cumsum(ds)) %>%
            ungroup()



        if (derivs == T) {
            distances <- distances %>% mutate(dsdt = ((s - lag(s)) / (time -
                                                                          lag(time))), d2sdt2 = ((dsdt - lag(dsdt)) /
                                                                                                     (time - lag(time)))) %>% ungroup()
        }
        if (print == T) {
            head(distances)
        }

        distances <-
            as.data.frame(distances) # being buggy with group by and gather. keep for now.

        return(distances)

    }
