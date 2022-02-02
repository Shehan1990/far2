#' File existancy
#'
#' Check if the file exists & let the user know that
#'
#' @param filename a csv file that contains the data
#'
#' @return This function returns reading the dataset. If the dataset is not available it informs about the unavailability
#'
#' @importFrom dplyr tbl_df
#' @importFrom readr read_csv
#'
#' @examples
#' \dontrun{
#' fars_read(accident_2013)
#' fars_read(accident_2014)
#' fars_read(accident_2013, accident_2015)
#' }
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


#' Make the file name
#'
#' Making the file name in which including the year
#'
#' @param year value indicating the year
#'
#' @return This function returns making the file name
#'
#' @examples
#' \dontrun{
#' make_filename(2013)
#' make_filename(2014)
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}


#' Read the years
#'
#' Configure the years of the data set & check the validity
#'
#' @param years value indicating the year/ years
#'
#' @return This function returns the "Month" & "Year". If incorrect year has entered it retunrs informing that the year is invalid.
#'
#' @importFrom dplyr select mutate
#' @import magrittr
#'
#' @examples
#' \dontrun{
#' fars_read_years(2013)
#' fars_read_years(2013, 2014, 2015)
#' }
#'
#' @export
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

#' Summary of the years
#'
#' Group the data by years & months together
#'
#' @param years value indicating the year/ years
#'
#' @return This function returns the number of records grouping by year & month
#'
#' @importFrom dplyr group_by summarize bind_rows
#' @importFrom tidyr spread
#' @import magrittr
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2013)
#' fars_summarize_years(2013, 2014)
#' }
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Make the map by states
#'
#' Visualize the data in the map
#'
#' @param state.num number of the state
#' @param year year for which injuries should be mapped
#'
#' @return This function returns the map of the given state
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @examples
#' \dontrun{
#' fars_map_state(43,2013)
#' fars_map_state(11,2014)
#' fars_map_state(3,2015)
#' }
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
