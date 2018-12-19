#' Read a FARS data file
#'
#' This function will read a raw FARS data file designated by the
#' input representing the FARS file name.
#'
#' @param filename A character string representing the name of a FARS file.
#' If the FARS data file is not in the working directory, then this argument must also
#'   include a relative path.
#'
#' @return This function returns a tbl_df object representing the data
#'    from a FARS raw data file, which has 52 columns of data.
#'
#' @examples
#' \dontrun{
#' # Using a correct filename as input ...
#' farsdata2015 <- fars_read("accident_2015.csv.bz2")
#'
#' # Using relative path and correct filename as input ...
#' farsdata2015 <- fars_read("mydata/accident_2015.csv.bz2")
#'
#' # It is also possible to use the make_filename function to create the
#' # filename, but the data file must be in the working directory in this example.
#' farsdata2015 <- fars_read(make_filename("2015"))
#'
#' # Using an invalid year will result in an error ...
#' fars1892 <- fars_read(make_filename("1892"))
#' #   Error in fars_read(make_filename("1892")) :
#' #   file 'accident_1892.csv.bz2' does not exist
#' }
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
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

#' Make a correctly formatted FARS file name
#'
#' This function will make a correctly formatted FARS file name using the
#' input that represents the year of the FARS data.
#'
#' @param year A character string representing the year of the FARS data.
#'   An integer value representing a year may also be used as input.
#'
#' @return This function returns a string representing the correctly
#'   formatted filename for the FARS data file.
#'
#' @examples
#' \dontrun{
#' # Using charactger string input ...
#' filename2015 <- make_filename("2015")
#' #  "accident_2015.csv.bz2"
#'
#' # Using integer input ...
#' filename2017 <- make_filename(2017)
#  #  "accident_2017.csv.bz2"
#' }
#'
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Read one or more FARS data files.
#'
#' This function will read one or more FARS data files designated by the input
#' years which can be a single year or a vector of years.
#'
#' @param years A character string or vector of character strings representing
#' the year(s) of the FARS data. An integer or vector of integers may also
#' be used for input; which represent the year(s) of the FARS data.
#'
#' @return This function returns a list containing all of the FARS data files
#' that were read. Each element of the list will be a tbl_df object containing
#' the FARS data corresponding to the order in which its year appeared in the
#' input parameter. Each tbl_df object will contain only two columns as follows:
#' 'Month' and 'year', corresponding to the FARS data object it represents.
#' Each row of each tbl_df object represents one fatality that occured in the
#' month noted and the year noted.
#'
#' @examples
#'  \dontrun{
#' # using character strings ...
#' farsYearsListA <- fars_read_years("2014")
#' farsYearsListB <- fars_read_years(c("2013","2015"))
#'
#' # using integers  ...
#' farsYearsListA <- fars_read_years(2014)
#' farsYearsListB <- fars_read_years(c(2013,2015))
#'
#' # using invalid year for the input will result in an error ...
#' farsYearsListBad <- fars_read_years("1894")
#' #  Warning message:
#' #  In value[[3L]](cond) : invalid year: 1894
#' }
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr  %>%
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

#' Summarize FARS data by year
#'
#' This function will summarize the FARS data for one or more years.
#'
#' @param years A character string or vector of character strings representing
#' the year(s) of the FARS data. An integer or vector of integers may also
#' be used for input; which represent the year(s) of the FARS data.
#'
#' @return This function returns a tbl_df object containing FARS summary data.
#' The first column is named 'MONTH' and will have an element for the integer value
#' of each month. The second and subsequent columns will be named by the year
#' of FARS data they represent, and each element in these columns will contain the
#' value of fatalities for that month.
#'
#' @examples
#' \dontrun{
#' # Using character strings as input ...
#' farsYearsListSummaryA <- fars_summarize_years("2014")
#' farsYearsListSummaryB <- fars_summarize_years(c("2013","2014","2015"))
#'
#' # Using integers as input ...
#' farsYearsListSummaryA <- fars_summarize_years(2014)
#' farsYearsListSummaryB <- fars_summarize_years(c(2013,2014,2015))
#'
#' # Using an invalid year will result in an error ...
#' farsYearsListSummaryC <- fars_summarize_years("1892")
#' # Error in grouped_df_impl(data, unname(vars), drop) :
#' # Column `year` is unknown
#' }
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr  %>%
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' Create a map of fatalities
#'
#' This function will create a map showing a single state and all the fatalies
#' for a given year. Each fatality will show up as a dot on a map.
#'
#' @param state.num An integer that is an identifier for a certain state.
#'
#' @param year A character string or integer representing the year of the
#' FARS data.
#'
#' @return This function will print a map to the graphics device and will
#'  return null
#'
#' @examples
#' \dontrun{
#' # map fatalities in Florida for 2014 and then for 2015
#' fars_map_state(12,"2014")
#' fars_map_state(12, 2015)
#'
#' # Using an invalid year will result in an error ...
#' fars_map_state(12,"1892")
#' # Error in fars_read(filename) :
#' # file 'accident_1892.csv.bz2' does not exist
#'
#' # Using an invalid state number will result in an error ...
#' fars_map_state(200,"2014")
#' # Error in fars_map_state(200, "2014") : invalid STATE number: 200
#' }
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
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
