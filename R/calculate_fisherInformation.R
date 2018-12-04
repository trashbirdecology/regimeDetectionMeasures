#' @title Fisher Information: three equations for calculating.
#'
#' @param dataIn A subset of data for each moving window. These data will be used to calculate the Fisher Information
#' @param fi.equation
#' @param min.window.dat
#' @export
#'
calculate_FisherInformation <- function(dataIn, min.window.dat = 2,  fi.equation =  "7.12") {

    if(fi.equation == 7.12){warning("Using FI equation 7.12. Calculating distances about input data ");
        dataIn = calculate_distanceTravelled(dataIn, derivs = T)
    }

    require(kedd)
    require(caTools)
    # Calculate distribution of distance travelled
    data <-
        dataIn %>%
        mutate(TT = max(time) - min(time),
               p = (1 / TT) * (1 / dsdt)) %>%
        na.omit(d2sdt2)

    if(nrow(data) <= min.window.dat) {
        warning("Two or less observations in window")
    }

    if (fi.equation == "7.3b") {
        # Equation 7.3b
        p <- data$p
        s <- data$s
        dp <- lead(p) - p
        ds <- lead(s) - s
        dpds <- dp / ds
        ind <- 1:(length(s) - 1)
        FI <- trapz(s[ind], (1 / p[ind]) * dpds[ind] ^ 2)

    } else if (fi.equation == "7.3c") {
        # Equation 7.3c
        q <- sqrt(data$p)
        s <- data$s
        dq <- lead(q) - q
        ds <- lead(s) - s
        dqds <- dq / ds
        ind <- 1:(length(s) - 1)
        FI <- 4 * trapz(s[ind], dqds[ind] ^ 2)

    } else if (fi.equation == "7.12") {
        # Equation 7.12

        t <- data$time
        s <- data$s
        TT <- max(t) - min(t)
        dsdt <- data$dsdt
        d2sdt2 <- data$d2sdt2
        ind <- 1:(length(s) - 1)
        FI <- (1 / TT) * trapz(t[ind], d2sdt2 ^ 2 / dsdt ^ 4)
    } else {

        warning("Unrecognized equation supplied (fi.equation must be one of c(7.3b, 7.3c, 7.12)")
        break

    }

    return(FI)


}
