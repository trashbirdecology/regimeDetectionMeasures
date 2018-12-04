#' @title Plot original data
#'
#' @param origData
#' @param example
#' @param
#' @param print print plots to device when print = T. Default print = T.
#' @param save NEEDS TO BE UPDATED TO CREATE LOCAL PLOT FOLDER AND EXPORT TO THIS FOLDER. JLB.
#' @return Function returns a data frame in long format with columns specifying site name, time (or spatial unit), variable (e.g. species identity), and value (e.g. species count).
#' @export
#'
#' @references
#' [1] Spanbauer, Trisha L., et al. "Prolonged instability prior to a regime shift." PLoS One 9.10 (2014): e108936.
#'
#' @examples
#' df <- munge_orig_dat()
#' plot_orig_data(df)
#'
#' @export
#'
plot_orig_data <-
    function(data,
             example = F,
             print = T,
             save = F) {
        p1 <- ggplot(data) +
            geom_line(aes(x = time, y = value, color = variable), size = .65) +
            theme_classic() +
            theme(legend.position = 'none')

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

        if (save == T) {
            return(p1)
        }

    }

# Plot species richness -----------------------------------
#' @title Plot species richness over time for the original data
#' @param origData
#' @param example
#' @param
#' @param print print plots to device when print = T. Default print = T.
#' @param save NEEDS TO BE UPDATED TO CREATE LOCAL PLOT FOLDER AND EXPORT TO THIS FOLDER. JLB.
#' @return Function returns a data frame in long format with columns specifying site name, time (or spatial unit), variable (e.g. species identity), and value (e.g. species count).
#' @export
#'
#' @references
#' [1] Spanbauer, Trisha L., et al. "Prolonged instability prior to a regime shift." PLoS One 9.10 (2014): e108936.
#'
#' @examples
#' df <- munge_orig_dat()
#' plot_richness(df)
#'
#' @export
#'
plot_richness <-
    function(data,
             example = F,
             print = T,
             save = F) {
        temp <- data %>%
            group_by(time) %>%
            filter(value > 0) %>%
            mutate(nSpp = n())

        p1 <- ggplot(data = temp, aes(y =  nSpp, x = time)) +
            geom_line() +
            theme_classic()


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

        if (save == T) {
            return(p1)
        }

    }




# Plot amount of (time) between samples -----------------------------------
#' @title Plot units of time elapsed between sampling points over the time series.
#'
#' @param origData
#' @param example
#' @param
#' @param print print plots to device when print = T. Default print = T.
#' @param save NEEDS TO BE UPDATED TO CREATE LOCAL PLOT FOLDER AND EXPORT TO THIS FOLDER. JLB.
#' @return Function returns a data frame in long format with columns specifying site name, time (or spatial unit), variable (e.g. species identity), and value (e.g. species count).
#' @export
#'
#' @references
#' [1] Spanbauer, Trisha L., et al. "Prolonged instability prior to a regime shift." PLoS One 9.10 (2014): e108936.
#'
#' @examples
#' df <- munge_orig_dat()
#' plot_timeDiff(df)
#'
#' @export
#'
plot_timeDiff <-
    function(data,
             example = F,
             print = T,
             save = F){
        temp <- data %>%
            group_by(time) %>%
            filter(value > 0) %>%
            select(-site,-value,-variable) %>%
            distinct(time, .keep_all = T) %>%
            arrange(time) %>%
            ungroup() %>%
            mutate(sampDiff = as.integer(round(abs(
                lead(time) - time
            ))))

        p1 <- ggplot(data = temp, aes(y =  sampDiff, x = time)) +
            geom_line() +
            theme_classic()


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

        if (save == T) {
            return(p1)
        }

    }


