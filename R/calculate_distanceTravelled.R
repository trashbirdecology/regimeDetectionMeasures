#' @title Calculate the `distance travelled` by the entire system. Also calculates the velocity and acceleration of the entire system over the time series.
#'
#' @param dataIn
#' @param derivs
#' @param print
#'
#' @export
#'
calculate_distanceTravelled <- function(dataIn, derivs = T, print = T) {  
    
    
    distances <- dataIn %>% group_by(variable) %>% arrange(variable,
                                                       time) %>% mutate(dx = value - lag(value)) %>% na.omit(dx) %>%
    ungroup() %>% 
    group_by(time) %>% 
    mutate(ds = sqrt(sum(dx ^ 2))) %>%
    ungroup() %>% 
    distinct(time, dx, ds) %>% 
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

distances<-as.data.frame(distances) # being buggy with group by and gather. keep for now. 
    
return(distances)
    
    }
