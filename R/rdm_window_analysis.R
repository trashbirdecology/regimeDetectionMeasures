#' @title Plot original data
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
      FI[i] <- NA
      VI[i] <- NA
  
}
time <- dataIn$time
timeSpan <- range(time)
TT <- timeSpan[2] - timeSpan[1]
winSize <- winMove * TT
warning(paste0("FYI: Each window will contain ", winSize, 
               " time units."))
winSpace <- max(lead(time) - time, na.rm = T)
warning(paste0("FYI: Each window will move forward by ~", 
               round(winSpace, digits = 5), " time units."))
winStart <- round(seq(min(dataIn$time), max(dataIn$time) - winSize, by = winSpace), 5)
winStop <- round(winStart + winSize, 5)
nWin <- length(winStart)
FI <- NULL
VI <- NULL
EWS <- NULL
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
    FI[i] <- calculate_FisherInformation(winData %>% 
                                           select(-site), fi.equation = fi.equation)
  }
  if ("VI" %in% to.calc) {
    VI[i] <- calculate_VI(winData,  fill)
  }
  if ("EWS" %in% to.calc) {
    EWS <- calculate_EWS(winData) %>% rbind(EWS)
  }
}
resultsOut = list()
resultsOut$FI_VI <- data_frame(winStart, winStop, FI, VI)
resultsOut$ews <- EWS
return(resultsOut)
    
    }
