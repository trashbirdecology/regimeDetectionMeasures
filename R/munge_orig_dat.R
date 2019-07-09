#' @title Munge the Original Data Frame.
#' @param data A data frame with ENTER DESCRIPTION
#' @param example Loads and munges data when parameters data = NULL & Default = T, running the function will load and munge data from Spanbauer et al. 2014.
#' @param fill Fills empty cells with this value. Default = NA. Consider using zero (0) as a fill if using species counts. Beware when using NA vs. zero. Default = 0
#' @return Function returns a data frame in long format with columns specifying site name, time (or spatial unit), variable (e.g. species identity), and value (e.g. species count).
#' @examples
#' munge_orig_dat() # will not write an object to environment
#' newDf = munge_orig_dat() # save the object to environment
#'
#' @references Spanbauer, Trisha L., et al. "Prolonged instability prior to a regime shift." PLoS One 9.10 (2014): e108936.
munge_orig_dat <- function(data = NULL, example = T, fill = NA) {

    if (is.null(data) & example == F) {
        stop("Must supply object `data` or specify `example = T`")
    } # error message

    if (example == T) {
        data = readr::read_csv(
            url(
                "http://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0108936.s001"
            )
        )

        origData <-
            data %>%
            dplyr::select(-Sample) %>%
            gather(variable, value, -YB1950) %>%
            rename(time = YB1950) %>%
            mutate(site = "Foy") %>%
            group_by(time, site) %>%
            ungroup() %>%
            mutate(value = as.numeric(value)) %>%
            dplyr::select(site, time, variable, value) %>%
            spread(-time,-site, fill = 0) %>%
            gather(variable, value,-time,-site)

    } # load example data if data = NULL & example = T


    if("site" %in% colnames(data)==F){
        data <- data %>%
            mutate(site = "NA")
    } # add `site` variable if it is missing

    if(is.null(data)){ #Munge original data if supplied
    origData <- data %>%
        dplyr::select(site, time, variable, value) %>%
        group_by(time, site) %>%
        mutate(value = as.numeric(value)) %>%

        spread(-time,-site, fill = fill) %>%
        gather(variable, value,-time,-site)
    }



    flag.index = names(origData)

    if(!("cellID" %in% flag.index)){
        flag1 = T
        origData <- origData %>%
            mutate(
                cellID = 1)
    }else(flag1 = F)

    if(!("sortVar" %in% flag.index)){
        flag2 = T
        origData <- origData %>%
            rename(sortVar = time)
    }else(flag2 = F)

    return(origData)


}



