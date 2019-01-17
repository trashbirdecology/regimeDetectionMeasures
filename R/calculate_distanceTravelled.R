#' @title Calculate the `distance travelled` by the entire system. Also calculates the velocity and acceleration of the entire system over the time series.
#'
#' @param dataIn
#' @param derivs
#' @param print
#'
#' @export
#'
calculate_distanceTravelled <- function(dataIn, derivs = T, print = T) {

    
    # Calculate distance traveled with special option for using BBS data identifier,
        distances <- dataIn %>% group_by(variable) %>% arrange(variable,
                                                               time) %>% mutate(dx = value - lag(value)) %>% na.omit(dx) %>%
          ungroup() %>% group_by(time) %>% summarise(ds = sqrt(sum(dx ^
                                                                            2))) %>%
          ungroup() %>%
          mutate(s = cumsum(ds))


    # Add the derivatives if desired
    if (derivs == T) {
      distances <- distances %>% mutate(dsdt = ((s - lag(s)) / (time -
                                                                  lag(time))),
                                        d2sdt2 = ((dsdt - lag(dsdt)) / (time -
                                                                          lag(time)))) %>% ungroup()
    }
    if (print == T) {
      head(distances)
    }
    return(distances)
    }

