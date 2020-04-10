#' @title Download, import, and munge example dataset.
#' @param fill Fills missing data with this value. Default = NA. __Consider using zero (0) if using species counts.__ Beware when using NA vs. zero. Default = 0
#' @return Returns a data frame in a 'long' format with columns 'site' (site id), 'sortVar' (the spatial or temporal index of interest; function will arrange the data according to this argument for analysis), 'variable' (unit of interest; e.g. species identity), and 'value' (measurement of 'variable'; e.g. species count).
#' @export
#' @examples
#' data <- get_example_data(example=TRUE) # save the object to environment
#'
#'
#'
#' @references Spanbauer, Trisha L., et al. "Prolonged instability prior to a regime shift." PLoS One 9.10 (2014): e108936.
get_example_data <- function(data = NULL, fill = NA) {


    data = readr::read_csv(
        url(
            "http://journals.plos.org/plosone/article/file?type=supplementary&id=info:doi/10.1371/journal.pone.0108936.s001"
        )
    )
    ## munge the example data (paleodiatom)
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
        gather(variable, value,-time,-site) %>%
        rename(sortVar = time)# rename sortvar



    if("site" %in% colnames(data)==FALSE){
        data <- data %>%
            mutate(site = "NA")
    } # add `site` variable if it is missing



        flag.index = names(origData)




    return(origData)


}



