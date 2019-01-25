#' @title Fisher Information: three equations for calculating.
#'
#' @param dataInFI A subset of data for each moving window. These data will be used to calculate the Fisher Information
#' @param fi.equation
#' @param min.window.dat
#' @export
#'
calculate_FisherInformation <-
    function(dataInFI,
             min.window.dat = 2,
             fi.equation =  "7.12") {
        FItemp <- NULL
        if (!("7.12" %in% fi.equation | "7.3b" %in% fi.equation |
              "7.3c" %in% fi.equation)) {
            warning("Unrecognized equation supplied (fi.equation must be one of c(7.3b, 7.3c, 7.12)")
        }
        if (!("dsdt" %in% colnames(dataInFI))) {
            dataInFI <- calculate_distanceTravelled(dataInFI, derivs = T)
        }
        require(kedd)
        require(caTools)

        dataInFI <- dataInFI %>% mutate(TT = max(time) - min(time),
                                        p = (1 / TT) * (1 / dsdt)) %>% filter(ds != 0)


        if (fi.equation == "7.3b" & nrow(dataInFI) > min.window.dat) {
            p <- dataInFI$p
            s <- dataInFI$s
            dp <- lead(p) - p
            ds <- lead(s) - s
            dpds <- dp / ds
            ind <- 1:(length(s) - 1)
            FItemp <- trapz(s[ind], (1 / p[ind]) * dpds[ind] ^ 2)
        }
        if (fi.equation == "7.3c" & nrow(dataInFI) > min.window.dat) {
            q <- sqrt(dataInFI$p)
            s <- dataInFI$s
            dq <- lead(q) - q
            ds <- lead(s) - s
            dqds <- dq / ds
            ind <- 1:(length(s) - 1)
            FItemp <- 4 * trapz(s[ind], dqds[ind] ^ 2)
        }
        if (fi.equation == "7.12" & nrow(dataInFI %>% na.omit(dsdt)) >
            min.window.dat) {
            dataInFI <- dataInFI %>% na.omit(dsdt)
            t <- dataInFI$time
            s <- dataInFI$s
            TT <- max(t) - min(t)
            dsdt <- dataInFI$dsdt
            d2sdt2 <- dataInFI$d2sdt2
            ind <- 1:(length(s) - 1)
            FItemp <- (1 / TT) * trapz(t[ind], d2sdt2 ^ 2 / dsdt ^ 4)
        }

        if (is.na(FItemp) | is.null(FItemp)) {
            FItemp        = NULL
            return(FItemp)
        }
        FItemp <-
            cbind(FItemp, min(dataInFI$cellID) , min(dataInFI$cellID))
        names(FItemp) <- c('FIvalue', 'cellID_min', 'cellID_max')
        return(FItemp)
    }
