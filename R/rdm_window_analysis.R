#' @title  Calculate the regime detection measured within a moving window.
#' @description This function is a wrapper for calculating various regime detection measures within a moving window.
#' @param dataInRDM A data frame with columns: sortVar: usually time or some spatial dimension; variable: usually species; value: observations about the variable (e.g., count)
#' @param winMove Number as proportion of each time series to be included in the moving windows. Default = 0.25 (25% of data included in each window).
#' @param min.windowdat Minimum # of data points in each window to include in calculations. Default = 2.
#' @param fill Used in the function 'calculate_VI'. Fill value for missing data. Default = 0
#' @param min.window.dat Used in calc_FisherInformation. Default = 2 data points
#' @param to.calc Which measures to calculate. VI variance index. FI Fisher Information. EWS 1st through 4th moments, etc. Default = ALL measures.
#' @export rdm_window_analysis

rdm_window_analysis <- function(dataInRDM,
                                winMove = 0.25,
                                min.window.dat = 2,
                                fi.equation = '7.12',
                                to.calc = c('VI', 'FI', 'EWS'),
                                fill = 0
){
    if (winMove > 1 | winMove < 1e-10) {
        stop("winMove must be a number between zero and one")
    }

    sortVar <- dataInRDM %>% distinct(sortVar) %>% arrange(sortVar)

    if (!is.vector(sortVar)) {
        sortVar = sortVar$sortVar
    }
#
#     if("cellID" %in% names(winData)){ winData = winData %>% dplyr::select(-cellID)}



    timeSpan <- range(sortVar)
    TT <- timeSpan[2] - timeSpan[1]
    winSize <- winMove * TT
      # message(paste0("FYI: Windows ~= ", winSize, " time units"))
    winSpace <- max(lead(sortVar) - sortVar, na.rm = T)
      # message(paste0("FYI: Windows advance by ~", round(winSpace,
    winStart <- round(seq(min(dataInRDM$sortVar), max(dataInRDM$sortVar) -
                              winSize, by = winSpace), 5)
    winStop <- round(winStart + winSize, 5)
    nWin <- length(winStart)
    FI <- NULL
    VI <- NULL
    EWS <- NULL

    # Loop
    for (i in 1:nWin) {
        winData <- dataInRDM %>% filter(sortVar >= winStart[i],
                                        sortVar < winStop[i]) %>% distinct()

        if (length(unique(winData$sortVar)) < min.window.dat | nrow(winData) <=
            min.window.dat) {
            warning("# time points < min.window.dat time points -- need more to calculate metrics.
                    Skipping current window.")
            next
        }
        if ("FI" %in% to.calc) {
            FI_temp = NULL
            FI_temp <- calculate_FisherInformation(dataInFI = winData,
                                                   fi.equation = fi.equation)


            FI_temp <- FI_temp %>%
                mutate(winStart = winStart[i], winStop = winStop[i],
                       metricType = paste0("FI_Eqn", fi.equation))

            FI <- rbind(FI_temp, FI)
        }
        if ("VI" %in% to.calc) {
            VI_temp = NULL
            VI_temp <- calculate_VI(winData, fill = fill) %>%
                as_tibble() %>% mutate(winStart = winStart[i],
                                       winStop = winStop[i], metricType = "VI")
            VI <- rbind(VI_temp, VI)
        }

        if ("EWS" %in% to.calc) {
            EWS <- calculate_EWS(winData) %>% rbind(EWS)
        }
    }

    # Append all results to a list
    resultsOut = list()
    if (!is.null(VI)) {
        resultsOut$VI <- VI %>% mutate(variable = "NA")
    }
    if (!is.null(FI)) {
        resultsOut$FI <- FI %>% mutate(variable = "NA")
    }
    if (!is.null(EWS)) {
        resultsOut$EWSs <- EWS
    }
    if (is.null(EWS) & is.null(VI) & is.null(FI)) {
        return(resultsOut = NULL)
    }

    resultsOut <- do.call(plyr::rbind.fill, lapply(resultsOut,
                                                   data.frame, stringsAsFactors = FALSE))

    return(resultsOut)
}
