#' @description Calculate the 'distance travelled' by the entire system.  Also calculates the velocity and acceleration of the entire system over the time series.
#' @title Calculate the 'distance travelled' by a multivariable system through phase space.
#' @param dataInDist A data frame containing the following columns: "Variable" is usually species identity;  "Value" is the observed value (e.g. count, density) of the variable; "sortVar" is the variable along which distance is calculated (e.g., time). The example data set is munged such that the sortVar column is named time.
#' @param derivs logical (default TRUE), calculates the velocity and acceleration of the distance travelled
#' @param print logical (default TRUE), prints output to device

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






              return(distances)

    }
