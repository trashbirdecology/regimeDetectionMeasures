#' @title Plot original data
#' @description This function plots variables as % relative abundance of variable *x* a time *t*. Missing observations are treated as zeroes. Variables with unit == zero are not considered at time *t*.
#' @param data A data frame.
#' @param example Logical. Specify "TRUE" if you would like to use example data for demonstration.
#' @param xLabel Option to change the xLabel on resultant ggplot from "time" to ...
#' @param print print plots to device when print = T. Default print = T.
#' @return Function returns a data frame in long format with columns specifying site name, time (or spatial unit), variable (e.g. species identity), and value (e.g. species count).
#' @references Spanbauer, Trisha L., et al. "Prolonged instability prior to a regime shift." PLoS One 9.10 (2014): e108936.
#' @examples
#' df <- munge_orig_dat()
#' plot_orig_data(df)
#' @export
plot_orig_data <-
    function(data,
             example = FALSE,
             print = TRUE,             xLabel = "time") {
        p1 <- ggplot(data) +
            geom_line(aes(x = sortVar, y = value, color = variable), size = .65) +
            theme_classic() +
            theme(legend.position = 'none') +
            xlab(xLabel)

        if (example == T) {
            p1 <- p1 +
                xlab("\nyears before 1942") +
                ylab("relative abundance\n") +
                scale_x_continuous(
                    labels = function(x)
                        paste0(x * -1)
                ) +
                theme(text = element_text(size = 18),
                      axis.text.x = element_text(angle =
                                                     90, hjust = 1))
        }

        if (print == T) {
            print(p1)
        }


    }

#' @title Plot species richness over time for the original data
#' @description This function plots the species richness over time as a function of the total unique variables within each time
#' @param data The original data frame
#' @param example Logical. If TRUE will use the paleodiatom data from Spanbauer et al. (2014), else will use input data.
#' @param print print plots to device when print = T. Default print = T.
#' @return Function returns a data frame in long format with columns specifying site name, time (or spatial unit), variable (e.g. species identity), and value (e.g. species count).
#' @param xLabel Option to change the xLabel on resultant ggplot from "time" to ...
#' @references Spanbauer, Trisha L., et al. "Prolonged instability prior to a regime shift." PLoS One 9.10 (2014): e108936.
#' @examples
#' df <- munge_orig_dat()
#' plot_richness(df)

plot_richness <-
    function(data,
             example = FALSE,
             print = TRUE,
             xLabel = "time") {
        temp <- data %>%
              group_by(sortVar) %>%
            filter(value > 0) %>%
            mutate(nSpp = n())

        p1 <- ggplot(data = temp, aes(y =  nSpp, x = sortVar)) +
            geom_line() +
            theme_classic() +
            xlab(xLabel)


        if (example == T) {
            p1 <- p1 +
                xlab("\nyears before 1942") +
                ylab("time since last observation\n") +
                theme(text = element_text(size = 18),
                      axis.text.x = element_text(angle =
                                                     90, hjust = 1)) +
                scale_x_continuous(
                    labels = function(x)
                        paste0(x * -1)
                )
        }

        if (print == T) {
            print(p1)
        }


    }




#' @title Plot units of time elapsed between sampling points over the time series.
#' This function plots the amount of time that has elapsed since the last sampled unit time.
#' @param data The original data frame
#' @param example Logical. If TRUE will use the paleodiatom data from Spanbauer et al. (2014), else will use input data.
#' @param print print plots to device when print = T. Default print = T.
#' @return Function returns a data frame in long format with columns specifying site name, time (or spatial unit), variable (e.g. species identity), and value (e.g. species count).
#' @references Spanbauer, Trisha L., et al. "Prolonged instability prior to a regime shift." PLoS One 9.10 (2014): e108936.

plot_timeDiff <-
    function(data,
             example = FALSE,
             print = TRUE,
             xLabel = "time") {
        temp <- data %>%
            # group_by(time) %>%
            filter(value > 0) %>%
            dplyr::select(-site, -value, -variable) %>%
            distinct(sortVar, .keep_all = T) %>%
            arrange(sortVar) %>%
            #ungroup() %>%
            mutate(sampDiff = as.integer(round(abs(
                lead(sortVar) - sortVar
            )))) %>%
            as_tibble()

        p1 <- ggplot(data = temp, aes(y =  sampDiff, x = sortVar)) +
            geom_line() +
            theme_classic() +
            xlab(xLabel)


        if (example == T) {
            p1 <- p1 +
                xlab("\nyears before 1942") +
                ylab("time since last observation\n") +
                theme(text = element_text(size = 18),
                      axis.text.x = element_text(angle =
                                                     90, hjust = 1)) +
                scale_x_continuous(
                    labels = function(x)
                        paste0(x * -1)
                )
        }

        if (print == T) {
            print(p1)
        }

    }
