% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{combine_forecasts}
\alias{combine_forecasts}
\title{Combine COVID19 and Influenza Forecasts}
\usage{
combine_forecasts(
  prof_data = NULL,
  dates_frcst_list = NULL,
  simdat_list = NULL
)
}
\arguments{
\item{prof_data}{- the complete data structure}

\item{dates_frcst_list}{- a list of length two with dates (fit and forecast) for each
pathogen}

\item{simdat_list}{- a list of length two with an array of trajectories for each pathogen}
}
\value{
a list with two items
simdat_both - a list of length two with the two estimates for the combined burden
(random and ordered)
inc_both - the combined reported hospitalization
dates_both - dates array for combined forecast
}
\description{
Forecasts are combined using two procedures: random addition of trajectories and perfect
ordering of trajectories
}
