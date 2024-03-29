% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{stat_fit}
\alias{stat_fit}
\title{Fit a baseline statistical model to the data}
\usage{
stat_fit(inc, ntraj)
}
\arguments{
\item{inc}{integer. Observed pathogen incidence}

\item{ntraj}{integer. Requested number of trajectories}
}
\value{
A 2D array of dimension (ntraj, ntimes) with the trajectories (ntimes is the incidence length)
}
\description{
See: file:///Users/michal/Downloads/pnas.2113561119.sapp.pdf for more details (start at bottom of page 3)
This baseline model forecasts a predictive median incidence equal to the incidence in the
most recent data point(s), with uncertainty around the median based on changes in incidence
that were observed in the past of the time series.

Note that we do not force the median of the forecast forecast to be equal to
the last observed value since the data is likely to be daily.  But we do truncate the distribution
so that it has no negative values.
Adjustments are made for daily data (e.g., we use the last seven data points for the forecast)
}
