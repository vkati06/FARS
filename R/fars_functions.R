#' @title Data Input
#'
#' @description Reads a csv file (sep = ,) into a tibble.
#'
#' @param filename The name (or path) of the (possibly compressed) file.
#'
#' @return This function returns a tibble. A warning tells you if the requested file does not exist.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @examples fars_read("accident_2015.csv.bz2")
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}


#' @title Helper Function
#'
#' @description This is a helper function used by other functions to read data.
#'
#' @note Not exported to the user.
#'
#' @param year An integer giving the year for which the file name is to be created.
#'
#' @return This function returns a character string of the file name corresponding to
#' the data for the given year.
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}


#' @title Helper Function
#'
#' @description This is a helper function used by the \code{fars_summarize_years} function to summarize data.
#'
#' @note Not exported to the user.
#'
#' @param years An integer or a vector of integers giving the year(s) the user is interested in.
#'
#' @return This function returns a list of tibbles. Each tibble corresponds to one year and contains
#' the months and year of each observation in the original data table.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom magrittr "%>%"
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}


#' @title Summary Statistics
#'
#' @description Number of observations per month per year.
#'
#' @param years An integer or a vector of integers giving the year(s) the user is interested in.
#'
#' @return This function returns a tibble containing the number of observations per month per year.
#' A warning tells you if the requested year is invalid.
#'
#' @importFrom dplyr bind_rows
#' @importFrom magrittr "%>%"
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize
#' @importFrom tidyr spread
#'
#' @examples fars_summarize_years(c(2013, 2015))
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' @title Map of Accidents
#'
#' @description Create a map of fars events for given state and year.
#'
#' @param state.num An integer giving the number of the state the function will display.
#' @param year An integer giving the year of which the function will display the map of accidents.
#'
#' @return This function returns a graphical object. In case of an invalid state number an error
#' message is displayed. A warning tells you if there are no accidents to plot.
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples fars_map_state(1, 2015)
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
