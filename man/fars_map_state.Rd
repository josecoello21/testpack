% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_map_state}
\alias{fars_map_state}
\title{The US National Highway Traffic Safety Administration}
\usage{
fars_map_state(state.num, year)
}
\arguments{
\item{state.num}{State number written as integer, 
the number cannot be less than one or greater than 56}

\item{year}{The year written either as an integer}
}
\description{
This function graphs the latitude and longitud of the selected state
}
\details{
An error message if the filename does not exists. 
   Stop and an error message if state.num is not in the data table. 
   Message in case no accidents occurred in that year/state
}
