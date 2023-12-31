% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{get_baseline}
\alias{get_baseline}
\title{Set value of baseline incidence}
\usage{
get_baseline(inc, end_of_season)
}
\arguments{
\item{inc}{incidence time series}

\item{end_of_season}{logical (TRUE or FALSE)}
}
\value{
baseline integer baseline value >= 1
}
\description{
Early in the season use first two weeks of data and later use last week
of data.  Early/late is determined by value of end_of_season
}
\keyword{internal}
