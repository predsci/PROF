% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{run_optim}
\alias{run_optim}
\title{Run the optim package to improve initial guess for parameters}
\usage{
run_optim(param0, inc)
}
\arguments{
\item{param0}{initial guess for model parameters}

\item{inc}{hospitalization incidence}
}
\value{
improved initial guess for some of the model parameters
}
\description{
For SIRH model use the optim package to obtain an improved initial
guess for some model parameters.  This is a simple optimization with a
fixed one value for the force of infection
}
\keyword{internal}
