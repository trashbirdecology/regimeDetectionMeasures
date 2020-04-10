#' @title Plot original data
#' @description This function plots variables as % relative abundance of variable *x* a time *t*. Missing observations are treated as zeroes. Variables with unit == zero are not considered at time *t*.
#' @param data A data frame with columns c(sortVar, variable, value)
#' @param example Logical. Specify "TRUE" if you are using the example data
#' @param x.lab Option to change the x.lab on resultant ggplot from "time" to ...
#' @param print print plots to device when print = TRUE. Default print = TRUE.
#' @export
#' @examples
#' @references Spanbauer, Trisha L., et al. "Prolonged instability prior to a regime shift." PLoS One 9.10 (2014): e108936.

plot_orig_data <-
    function(data,
             example = FALSE,
             print = TRUE,
             x.lab = "time") {
        p1 <- ggplot(data) +
            geom_line(aes(x = sortVar, y = value, color = variable), size = .65) +
            theme_classic() +
            theme(legend.position = 'none') +
            xlab(x.lab)

        if (example == TRUE) {
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

        if (print == TRUE) {
            print(p1)
        }


    }
