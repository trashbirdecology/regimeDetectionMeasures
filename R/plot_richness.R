
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



