#' @title Plot original data
#' @param dataIn A data frame in long format. See description **NEED TO UPDATE THIS!**
#' @param winMove Number as proportion of each time series to be included in the moving windows. Default = 0.25 (25% of data included in each window).
#' @param min.windowdat Minimum # of data points in each window to include in calculations. Default = 2.
#' @param overrideSiteErr
#' @param fill Used in the function 'calculate_VI()'. Fill value for missing data. Default = 0
#' @param min.window.dat Used in calc_FisherInformation. Default = 2 data points
#'@param to.calc Which measures to calculate. VI variance index. FI Fisher Information. EWS 1st through 4th moments, etc. Default = ALL measures.
#' @export
rdm_window_analysis <- function(dataIn,
                                winMove = 0.25,
                                overrideSiteErr = F,
                                min.window.dat = 2,
                                fi.equation = '7.12',
                                to.calc = c('VI', 'FI', 'EWS'),
                                fill = 0
){
  if (winMove > 1 | winMove < 1e-10) {
    stop("winMove must be a number between zero and one")
  }
  if (length(unique(dataIn$time)) < 5) {
    next("Five or less time points in the data frame")
  }

  # Keep and sort unique time for partitioning windows
   time <- dataInRDM %>% distinct(time) %>% arrange(time)
   if(!is.vector(time)){
       time = time$time
   }


  timeSpan <- range(time)
  TT <- timeSpan[2] - timeSpan[1]
  winSize <- winMove * TT
  message(paste0("FYI: Windows ~= ", winSize,
                 " time units"))
  winSpace <- max(lead(time) - time, na.rm = T)
  message(paste0("FYI: Windows advance by ~",
                 round(winSpace, digits = 5), " time units."))
  winStart <- round(seq(min(dataIn$time), max(dataIn$time) - winSize, by = winSpace), 5)
  winStop <- round(winStart + winSize, 5)
  nWin <- length(winStart)
  FI <- c()
  VI <- c()
  EWS <- NULL


  ## Begin window for-loop to analyze FI, VI and EWSs
  for (i in 1:nWin) {

    winData <- dataIn %>% filter(time >= winStart[i], time <
                                   winStop[i]) %>%
      distinct()
    # Leave loop if not enough data points
    if (length(unique(winData$time)) < min.window.dat | nrow(winData) <= min.window.dat) {
      warning("Fewer than min.window.dat time points -- need more to calculate metrics. Skipping window.")
      next
    }
    # Calcuate the metrics if in argument to.calc
    if ("FI" %in% to.calc) {
      FI_temp = NULL
      FI_temp <- calculate_FisherInformation(dataInFI = winData, fi.equation = fi.equation) %>%
        as_tibble() %>%
        mutate(winStart = winStart[i],
               winStop = winStop[i],
               metricType = paste0("fiEqn_", fi.equation),
               site = unique(winData$site))

      FI <- rbind(FI_temp, FI)

      }
    if ("VI" %in% to.calc) {
      VI_temp = NULL
      VI_temp <- calculate_VI(winData,  fill = fill) %>%
        as_tibble() %>%
        mutate(winStart = winStart[i],
               winStop = winStop[i],
               site = unique(winData$site),
               metricType = "VI")

      VI <- rbind(VI_temp, VI)

    }
    if ("EWS" %in% to.calc) {
      EWS <- calculate_EWS(winData) %>% rbind(EWS)
    }

  }  # End nWin loops


# Create new df -----------------------------------------------------------


  resultsOut = list()
  if(!is.null(VI)){
  resultsOut$VI <- VI %>%  mutate(variable = 'NA')
  }

  if(!is.null(FI)){
    resultsOut$FI <- FI %>%  mutate(variable = 'NA')
  }

if (!is.null(EWS)) {
    resultsOut$EWSs <- EWS %>% tidyr::gather(key = "metricType",
                                           value = "value", -site, -variable, -winStart, -winStop)
}


  resultsOut <- do.call(plyr::rbind.fill, lapply(resultsOut, data.frame, stringsAsFactors=FALSE)) %>%
    dplyr::rename(metricValue = value)

  return(resultsOut)

}
