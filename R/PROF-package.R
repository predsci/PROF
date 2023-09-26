#' @importFrom utils write.csv read.csv download.file
#' @importFrom zoo rollmean
#' @import EpiEstim
#' @import ggplot2
#' @import plyr
#' @import dplyr
#' @import lubridate
#' @import pomp
#' @import gridExtra
#' @importFrom stats dpois optim rpois runif
#' @importFrom graphics  par
#' @importFrom grDevices dev.off pdf
#' @importFrom yaml read_yaml write_yaml
#' @useDynLib PROF, .registration = TRUE
#' @details
#' Additional details...
#'

#' @exportPattern ^[[:alpha:]]+
## usethis namespace: start
## usethis namespace: end
## NULL
#' @keywords internal
"_PACKAGE"


#' U.S. location FIPS codes and population info.
#'
#' A dataset containing the FIPS codes and populations of 53 states and
#'  territories as well as the national population. The variables are as follows:
#'
#' \itemize{
#'   \item abbreviation. Two-letter location abbreviation
#'   \item location. Two-digit location FIPS code
#'   \item location_name. Full name of location
#'   \item population. Integer estimate of location population
#' }
#'
#' @docType data
#' @keywords datasets
#' @name loc_pops
#' @usage data(loc_pops)
#' @format A data frame with 54 rows and 4 variables
NULL

