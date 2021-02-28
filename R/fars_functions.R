#' Read Fatality Analysis Reporting System
#' 
#' This is a function that reads data from US National Highway 
#' Traffic Safety Administration's found in the working directory
#' 
#' @param filename A string of characters indicating the name of 
#'    the file to be loaded
#'
#' @return This function returns a tibble of n observations and
#'    m columns from the selected file
#'    
#' @details an error message if the file does not exist 
#' 
#' @example  fars_read('accident_2013.csv.bz2')
#' 
#' @import readr
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

#' Data names from the US National Highway Traffic Safety 
#' Administration
#' 
#' This function creates the name of the US National Highway 
#' Traffic Safety Administration's data set for the selected year
#' 
#' @param year The year written either as an integer
#'
#' @return This function returns a string of characters with the 
#'    name of the data for the selected year
#' 
#' @example  make_filename(2013)
#' 
#' @export

make_filename <- function(year) {
  year <- as.integer(year)
  sprintf("accident_%d.csv.bz2", year)
}

#' Months and year of data from the US National Highway Traffic 
#' Safety Administration.
#' 
#' This function reads data from the US National Highway Traffic 
#' Safety Administration and returns the variables month and year
#' 
#' @param years The years written as an integer
#'
#' @return This function returns a list containing one or some tibbles of n 
#'    observations and two variables that are MONTH and year
#' 
#' @example  fars_read_years(2013) or fars_read(2013,2014,...)
#' 
#' @import magrittr
#' @import dplyr
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

#' Fatal injuries suffered in motor vehicle traffic crashes.
#' 
#' This function summarizes fatal injuries sustained in motor 
#' vehicle traffic accidents by month
#' 
#' @inheritParams fars_read_years
#'
#' @return This function returns a tibble of twelve observations  
#'    and n variables that are MONTH and YEAR with the summary 
#'    of the number of fatal injuries per month
#'    
#' @details Returns NULL if the year is invalid or not found in the data
#' 
#' @example  fars_summarize_years(2013)
#' 
#' @import magrittr
#' @import dplyr
#' @import tidyr
#' 
#' @export

fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>% 
    dplyr::group_by(year, MONTH) %>% 
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#' The US National Highway Traffic Safety Administration
#' 
#' This function graphs the latitude and longitud of the selected state
#' 
#' @param state.num State number written as integer, 
#'    the number cannot be less than one or greater than 56
#'
#' @inheritParams make_filename
#' 
#' @details An error message if the filename does not exists. 
#'    Stop and an error message if state.num is not in the data table. 
#'    Message in case no accidents occurred in that year/state
#' 
#' @example fars_map_state(1,2013)
#' 
#' @import dplyr
#' @import maps
#' @import graphics
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
