#' @description Calculate the 'distance travelled' by the entire system.  Also calculates the velocity and acceleration of the entire system over the time series.
#' @title Calculate the 'distance travelled' by a multivariable system through phase space.
#' @param long.format logical (default = TRUE); outputs the data in long rather than wide format.
#' @param data A data frame containing the following columns: "Variable" is usually species identity;  "Value" is the observed value (e.g. count, density) of the variable; "sortVar" is the variable along which distance is calculated (e.g., time). The example data set is munged such that the sortVar column is named time.
#' @param get.derivs logical (default = TRUE), calculates and appends the derivatives of the distance travelled, velocity (first) and acceleration (second).
#' @param print logical (default = TRUE), prints output to plotting device.
#' @export

calc_distance <-
    function(data,
             get.derivs = TRUE,
             long.format=TRUE,
             print = TRUE
             ) {

if("site" %in% names(data)){
group_args<-c("site", "variable");
arrange_args <- c("site", "variable", "sortVar")
}else{
    group_args <-c("variable");
    arrange_args <- c("variable", "sortVar")
}

    distances <- data %>%
            group_by_at(group_args) %>%
            arrange_at(arrange_args) %>%
            mutate(dx = value - lag(value)) %>%
            na.omit(dx) %>%
            ungroup() %>%
            group_by(sortVar) %>%
            mutate(ds = sqrt(sum(dx ^ 2))) %>%
            ungroup() %>%
            distinct(sortVar,  ds, .keep_all = T) %>%
            dplyr::select(sortVar, ds) %>%
            arrange(sortVar) %>%
            mutate(s = cumsum(ds)) %>%
            ungroup()


        if (get.derivs == TRUE) {
            distances <- distances %>% mutate(dsdt = ((s - lag(s)) / (sortVar -
                                                                          lag(sortVar))), d2sdt2 = ((dsdt - lag(dsdt)) /
                                                                                                        (sortVar - lag(sortVar)))) %>% ungroup()
        }
        if (print == TRUE) {
            head(distances)
        }

    if(long.format){
        distances <- distances %>%
        gather(key=metricType, value=metricValue, -sortVar)
    }

        return(distances)

    }
