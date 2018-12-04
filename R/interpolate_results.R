#' @title Linearally Interpolate the Results for Distance Travelled
#' @param timeOut The number of time units over which we will interpolate (with equal spacing).
#' @param data A data frame containing results from `calculate_distanceTravelled()`
#' @export
interpolate_distance <- function(data, timeOut) {
    newTime <-
        seq(min(data$time, na.rm = T),
            max(data$time, na.rm = T),
            length.out = timeOut)

    interpolated <- data %>%
        mutate(prob = as.numeric(as.character(prob)),
               winMove = as.numeric(as.character(winMove))) %>%
        # Gather variables into long form
        gather(key, value, -method, -prob, -nDraw, -time, -winMove) %>%
        # Create a new column indicating group
        unite(myGroup, c("method", "prob", "key", "nDraw", "winMove"), remove = FALSE) %>%
        # Split by group variable to create list
        split(.$myGroup)


    # Remove data subsets with too few data to properly interpolate
    cond <- sapply(interpolated, function(x) nrow(x) >=5)
    interpolated <- interpolated[cond]


    # Interpolate each variable
    interpolated <- interpolated  %>%
        map(~ approx(.$time, .$value, xout = newTime)$y) %>%
        # Bind interpolated results and convert back to data frame
        do.call(cbind, .) %>%
        as_tibble()

    interpolated <- interpolated  %>%
        # Add new time variable
        mutate(time = newTime) %>%
        # Gather groups back into long form
        gather(myGroup, value, -time) %>%
        # Separate group variable
        separate(myGroup, c("method", "prob", "key", "nDraw", "winMove"))

    avgInterpolated <-
        interpolated %>%
        # find mean, sd, lower and upper prediction intervals for each interpolated key value
        group_by(time, method, prob, key, winMove) %>%
        summarise(
            sdVal = sd(value, na.rm = T),
            meanVal = mean(value, na.rm = T),
            lb = quantile(value, 0.025, na.rm = T),
            ub = quantile(value, 0.975, na.rm = T)
        ) %>%
        ungroup() %>%
        na.omit(sdVal)

    return(avgInterpolated)
}



# Function for VI and FI dataframe ----------------------------------------------------------------
#' @title Linearally Interpolate the Results for Distance Travelled
#' @param timeOut The number of time units over which we will interpolate (with equal spacing).
#' @param data A data frame containing results from `calculate_distanceTravelled()`
#' @export
interpolate_FI_VI <- function(EWS, timeOut, winMove) {

    # create df containing unique variable IDs
    sppID <- EWS %>%
        mutate(sppID = EWS %>% group_indices(variable)) %>%
        select(sppID, variable)

    # Replace varialbe names with this ID in OD
    ewsResults <- EWS %>%
        mutate(sppID = sppID$sppID) %>%
        dplyr::select(-variable)


    newTime <-
        seq(min(ewsResults$time, na.rm = T),
            max(ewsResults$time, na.rm = T),
            length.out = timeOut)

    myDataInterp1 <- ewsResults %>%
        mutate(prob = as.numeric(as.character(prob))*100,
               winMove = 100*as.numeric(as.character(winMove))) %>%
        # Gather variables into long form
        gather(key, value, -method, -prob, -nDraws, -time, -winMove, -sppID) %>%
        # Create a new column indicating group
        unite(myGroup, c("sppID", "key", "method", "prob","nDraws", "winMove"), remove = FALSE) %>%
        # Split by group variable to create list
        split(.$myGroup)

    cond <- sapply(myDataInterp1, function(x) nrow(x) >=5)
    myDataInterp1 <- myDataInterp1[cond]

    # Interpolate each variable
    myDataInterp2 <- myDataInterp1  %>%
        map(~ approx(.$time, .$value, xout = newTime)$y) %>%
        # Bind interpolated results and convert back to data frame
        do.call(cbind, .) %>%
        as_tibble()

    myDataInterp3 <- myDataInterp2  %>%
        # Add new time variable
        mutate(time = newTime) %>%
        # Gather groups back into long form
        gather(myGroup, value, -time) %>%
        # Separate group variable
        separate(myGroup, c("sppID" , "key", "method", "prob","nDraws", "winMove"))


    myDataInterp4 <-
        myDataInterp3 %>%
        group_by(sppID, method, prob, time, key, winMove) %>%
        summarise(
            sdVal = sd(value, na.rm = T),
            meanVal = mean(value, na.rm = T),
            lb = quantile(value, 0.025, na.rm = T),
            ub = quantile(value, 0.975, na.rm = T)
        ) %>%
        ungroup()%>%
        na.omit()


    myDataAvg <- myDataInterp4 %>%
        mutate(key = as.factor(key),
               method = as.factor(method),
               prob = as.numeric(prob),
               winMove = as.integer(winMove)) %>%
        # remove species that have no observations..
        group_by(sppID, method, prob, key, winMove) %>%
        filter(sum(meanVal) > 0) %>%
        ungroup()

    myDataAvg$sppID = as.integer(myDataAvg$sppID)

    dataOut <- left_join(myDataAvg, sppID)


    return(dataOut)

}

