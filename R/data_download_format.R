COLS <- list("date", "state", "previous_day_admission_influenza_confirmed",
          "previous_day_admission_influenza_confirmed_coverage",
          "previous_day_deaths_influenza",
          "previous_day_deaths_influenza_coverage",
          "previous_day_admission_adult_covid_confirmed",
          "previous_day_admission_adult_covid_confirmed_coverage",
          "previous_day_admission_pediatric_covid_confirmed",
          "previous_day_admission_pediatric_covid_confirmed_coverage",
          "deaths_covid", "deaths_covid_coverage")

DEF_FILE_BASE <- "HHS_daily-hosp_state"

FROM_TIMESTAMP_FMT <- "%a, %d %b %Y %H:%M:%S GMT"

TO_TIMESTAMP_FMT <- "%y%m%d%H%M%S"

SUPPORTED_SEASONS <- c(2021, 2022, 2023)

API <- "https://healthdata.gov/resource/g62h-syeh.csv"

#' @title Download daily state-level HHS PROTECT hospitalization admission data
#' to a CSV.
#'
#' @description Function returns the filepath where the HHS csv file was downloaded to,
#' the date the dataset was last modified (as reported by healthdata.gov),
#' and a flag corresponding to the status code of the API call. The data
#' fetched through `fetch_hhs_data` is accessed through the HealthData.gov API,
#' supported by the Socrata framework. As such, the API utilizes SoQL
#' syntax to construct a DB query and retrieve the relevant data. The
#' function signature for `fetch_hhs_data` wraps the Socrata API,
#' constructing a SoQL query of the form:
#'      SELECT `fields` FROM `API`
#'      WHERE `conditions`
#'      ORDER BY `order` ASC
#'      LIMIT `limit`
#'
#' NOTE: When no `down_filename` is provided, the function creates a filename
#' HHS_daily-hosp_state__<last_modified>.csv, where `last_modified` is a POSIX
#' timestamp of the form: YYmmddHHMMSS.
#'
#' @param down_dir character string. The directory path to download to.
#' @param down_filename character string. The filename to download to.
#' @param fields character vector. Fields included in GET query.
#' @param order character string. Field to order the returned dataset on.
#' @param limit integer. Maximum number of records returned by GET query.
#' @param conditions character string. WHERE clause used in DB query.
#'
#' @return list. Named list containing: download_path, last_modified, and out_flag
#' @export
#'
#' @examples
#' fetch_hhs_data(down_dir = "data")
#' fetch_hhs_data(down_dir = "data",
#'                fields=c("date", "state", deaths_covid"),
#'                conditions="state == 'CA' AND deaths_covid IS NOT NULL")
#'
fetch_hhs_data <- function(down_dir="~",
                           down_filename=NULL,
                           fields=COLS,
                           order="date",
                           limit=1000000,
                           conditions=NULL) {

  return_data <- list(download_path=NULL, last_modified=NULL, out_flag=1)

  if (!dir.exists(down_dir)) {
    cat("'", down_dir, "'", "directory does not exist\n")
    return(return_data)
  }

  query <- list()
  if (!is.null(fields)) {
    query$`$select` <- paste(fields, collapse = ",")
  }
  if (!is.null(order)) {
    query$`$order` <- as.character(order)
  }
  if (!is.null(limit)) {
    # query$`$limit` <- as.character(limit)
    query$`$limit` <- format(limit, scientific=FALSE)
  }
  if (!is.null(conditions)) {
    query$`$where` <- as.character(conditions)
  }

  # Perform API request
  # If query dictionary has been populated, pass this as an argument to GET call
  if (length(query) == 0) {
    response <- httr::GET(API)
  } else {
    response <- httr::GET(API, query=query)
  }

  # Check response status
  if (response$status_code == 200) {
    posix_timestamp <- as.POSIXct(response$headers$`last-modified`, format = FROM_TIMESTAMP_FMT, tz = "GMT")

    # Generate default download_path of the form:
    # HHS_daily-hosp_state__<last_modified>.csv

    # This default naming protocol can be used in conjunction with the function
    # `filename_timestamp_to_posix` to read in the `last_modified` metadata pertaining
    # to a dataset, and determine if a more recent dataset is available from the
    # healthdata.gov API.
    if (is.null(down_filename)) {
      fmt_timestamp <- format(posix_timestamp, TO_TIMESTAMP_FMT)
      down_filename <- paste(DEF_FILE_BASE, "__", fmt_timestamp, ".csv", sep = "")
    }

    filepath <- file.path(down_dir, down_filename)
    data <- read.csv(text = httr::content(response, "text"))

    # "https://healthdata.gov/resource/g62h-syeh.csv" returns the `date` column as a
    # datetime type. PROF requires the `date` column to be a date type, not datetime.
    # As such, if `date` is included, it is cast into the form: YYYY-mm-dd.
    if ("date" %in% colnames(data)) {
      data$date <- as.Date(data$date, format = "%Y-%m-%d")
    }

    write.csv(data, filepath, row.names = FALSE)

    if (file.exists(filepath)) {
      cat("\nData Saved At:", filepath, "\n\n")
      return_data$download_path <- filepath
      return_data$last_modified <- posix_timestamp
      return_data$out_flag <- 0

      # Clean out existing dataset. For now, this only looks for csv files that
      # have been generated through the function's default filepath naming
      # conventions, viz. using the "__<last_modified>.csv" as an identifier for
      # possible matches.
      # This functionality has been commented out pending approval.

      file_pattern <- paste(DEF_FILE_BASE, "__[0-9]+\\.csv$", sep = "")
      files <- list.files(down_dir, pattern = file_pattern)
      for (file in files) {
        if (file != down_filename) {
          file.remove(file.path(down_dir, file))
          cat("Removing File:", file, "\n")
        }
      }
    }

    return(return_data)
  } else {
    cat(httr::content(response, as="text"))
    return(return_data)
  }
}


#' @title Convert POSIX timestamp from filename to POSIXct object.
#'
#' @description This function extracts a POSIX timestamp from a filename following a specific pattern,
#' then converts it to a POSIXct object.
#'
#' @param filepath Character string. The filepath containing the filename with the POSIX timestamp.
#'
#' @return A POSIXct object representing the timestamp extracted from the filename.
#' @export
#'
#' @examples
#' filepath <- "HHS_daily-hosp_state__210226120000.csv"
#' posix_timestamp <- filename_timestamp_to_posix(filepath)
#' posix_timestamp
#'
filename_timestamp_to_posix <- function(filepath) {
  pattern <- paste(DEF_FILE_BASE, "__[0-9]{12}\\.csv$", sep = "")
  filename <- regmatches(filepath,regexpr(pattern, filepath))
  if (length(filename) != 0) {
    lm_timestamp <- regmatches(filename,regexpr("[0-9]{12}", filename))
    posix_timestamp <- as.POSIXct(lm_timestamp, format = TO_TIMESTAMP_FMT, tz = "GMT")
    return(posix_timestamp)

  } else {
    cat("ERROR: Improper filename structure.\nCannot extract POSIX timestamp.")
    return()
  }
}

#' @title HHS Last Modified Date
#' @description Fetch the date (in GMT) that the `g62h-syeh` dataset was last modified, as
#' reported by healthdata.gov
#'
#' @return POSIXct Date. Date (where tz=GMT) that the `g62h-syeh` dataset was
#'         last modified. If the GET response code is not 200, NULL is returned.
#' @export
#'
#' @examples
#' filepath <- "HHS_daily-hosp_state__210226120000.csv"
#' posix_timestamp <- filename_timestamp_to_posix(filepath)
#' last_modified <- fetch_hhs_last_modified()
#' posix_timestamp < last_modified
#'
fetch_hhs_last_modified <- function() {
  query <- list()
  query$`$select` <- "date"
  query$`$limit` <- "0"
  response <- httr::GET(API, query=query)
  if (response$status_code == 200) {
    posix_timestamp <- as.POSIXct(response$headers$`last-modified`,
                                  format = FROM_TIMESTAMP_FMT, tz = "GMT")
    return(posix_timestamp)
  } else {
    return()
  }
}


#' Fetch the start and end dates for a given `season`.
#'
#' @param season integer. The requested season.
#' @return list of Dates. Contains elements `t0` and `t1` corresponding to
#'         the season's start date and end date, respectively.
#' @export
#'
#' @examples
#' date_range <- fetch_season_tstart_tend(2023)
#'
fetch_season_tstart_tend <- function(season) {
  if(!(as.character(season) %in% SUPPORTED_SEASONS)) {
    stop('\nRequested season is not supported\n')
  }
  start_date = as.Date(paste0(season,'-09-01'), format = "%Y-%m-%d")
  end_date   = as.Date(paste0(season+1,'-06-01'), format = "%Y-%m-%d")

  if (season == 2023) {
    start_date = as.Date(paste0(season,'-08-01'), format = "%Y-%m-%d")
  }

  return (list(t0=start_date, t1=end_date))
}


#' @title Read a User provided incidence file and populate the PROF data structure
#' @description Given a path to a User provided data file, population size, and
#' location name populate the PROF data structure.
#' Data must contain the following fields:
#' date (%Y-%m-%d format), disease (string, covid19, influenza), metric (string, 'hosp'),
#' value (numeric, incidence)
#' @param filepath string.  Path to csv data file
#' @param population numeric.  Population size  (no default)
#' @param location string. Location name (default is "PROFVille")
#' @return PROF data structure
#' @export
#'
csv_to_prof <- function(filepath, population, location="PROFVille") {

  if (file.exists(filepath)) {
    raw_csv <- read.csv(file=filepath, stringsAsFactors=T)
  } else {
    stop(paste('\nFile', filepath, 'does not exist\n'))
  }

  if (!all(names(raw_csv) %in% c("date", "disease", "metric", "value"))) {
    stop('\nData must contain the fields: date, disease, metric, value\n')
  }

  if (is.null(population) | population < 0) {
    stop("\nPlease provide population size as a positive integer.\n")
  }

  raw_csv$date <- ymd(raw_csv$date)


  # if (is.null(fit_start)) {
  #   fit_start <- min(raw_csv$date)
  # }
  # if (is.null(fit_end)) {
  #   fit_end <- max(raw_csv$date)
  # }
  # if (as.Date(fit_start) > as.Date(fit_end)) {
  #   stop('\nFit start date is greater than the fit end date\n')
  # }

  season_start <- min(raw_csv$date)
  season_end   <- max(raw_csv$date)

  if (is.na(location) | is.null(location)) location <- "PROFVille"

  prof_data <- list()

  season_mask <- (raw_csv$date >= as.Date(season_start)) &
    (raw_csv$date <= as.Date(season_end))

  for (disease_level in levels(raw_csv$disease)) {
    disease_mask <- (raw_csv$disease == disease_level) & season_mask
    inc_df = raw_csv[disease_mask, c('date', 'value')]
    inc_df = inc_df[order(inc_df$date), ]
    names(inc_df) = c('date', 'inc')

    prof_data[[disease_level]] <- list()
    prof_data[[disease_level]]$population <- population
    prof_data[[disease_level]]$disease <- disease_level
    prof_data[[disease_level]]$loc_name <- location
    prof_data[[disease_level]]$inc_type <- levels(raw_csv$metric)[1]
    prof_data[[disease_level]]$data <- inc_df
    # prof_data[[disease_level]]$data_fit <- inc_df[(inc_df$date >= as.Date(fit_start)) &
    #                                               (inc_df$date <= as.Date(fit_end)), ]
  }

  return (prof_data)
}


# #' Download daily state-level HHS PROTECT hospitalization admission data
# #' to a CSV.
# #'
# #' @param down_dir character string. The directory path to download to.
# #' @param down_filename character string. The filename to download to.
# #'
# #' @return an integer. 0 for success and non-zero for failure.
# #' @export
# #'
# #' @examples
# #' hhs_hosp_state_down(down_dir = "~/Downloads", down_filename = NULL)

# hhs_hosp_state_down <- function(down_dir="~/", down_filename=NULL) {
#
#
#   api_url = "https://healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD"
#
#   if (is.null(down_filename)) {
#     save_filename_base = "HHS_daily-hosp_state"
#     today_date = Sys.Date()
#     down_filename = paste0(save_filename_base, ".csv")
#   }
#
#   # download the new file (overwriting the previous current file)
#   download_path = file.path(down_dir, down_filename)
#   cat("Attempting download at time: ", format(Sys.time()), "\n", sep="")
#   out_flag = download.file(url=api_url, destfile=download_path)
#
#   return(list(download_path=download_path, out_flag=out_flag))
# }


#' @title Open the HHS Protect CSV.
#'
#' @description Read the file with stringsAsFactors=TRUE, and return a dataframe.
#' @param hhs_file Character string. Path to CSV file.
#'
#' @return dataframe of all HHS PROTECT columns
#' @export
#'
#' @examples
#' load_HHS_csv(hhs_file = NULL)
#'
load_HHS_csv <- function(hhs_file=NULL) {
  # open raw data file
  hosp_data = read.csv(file=hhs_file, stringsAsFactors=T)
  return(hosp_data)
}


#' @title Subset HHS PROTECT dataset
#' @description Subset HHS PROTECT hospitalizations by state, metric, and dates.
#'
#' @param hosp_data dataframe. Full HHS PROTECT dataset.
#' @param get_cols vector of strings. The names of the HHS PROTECT columns to
#' be extracted.
#' @param state character vector (length=2). Abbreviation of the state to be
#' subset.
#' @param start_date Date. Start date (inclusive) for data subsetting.
#' @param end_date Date. End date (inclusive) for data subsetting.
#' @param shift_dates logical (FALSE). Previous versions of the HHS data listed
#' admissions by day reported, which required a one-day shift to be
#' 'day of admission'.
#'
#' @return Dataframe. This has columns 'state', 'data', and everything specified
#' in get_cols. Rows are sorted by date. The dataframe contains only data
#' for the specified state.
#' @export
#'
#' @examples
#' mycols = get_cols=c("previous_day_admission_adult_covid_confirmed",
#' "previous_day_admission_adult_covid_confirmed_coverage",
#' "previous_day_admission_pediatric_covid_confirmed",
#' "previous_day_admission_pediatric_covid_confirmed_coverage",
#' "deaths_covid", "deaths_covid_coverage")
#'
#' get_HHS_state(hosp_data = NULL, get_cols = mycols, state = "CA",
#' start_date = NULL, end_date = NULL, shift_dates = F)
#'
get_HHS_state <- function(hosp_data=NULL, get_cols=c(
  "previous_day_admission_adult_covid_confirmed",
  "previous_day_admission_adult_covid_confirmed_coverage",
  "previous_day_admission_pediatric_covid_confirmed",
  "previous_day_admission_pediatric_covid_confirmed_coverage",
  "deaths_covid", "deaths_covid_coverage"),
                          state="CA", start_date=NULL,
                          end_date=NULL, shift_dates=F)
  {

  # all_dates = sort(as.Date(levels(hosp_data$date)), decreasing=FALSE)
  # all_states = levels(hosp_data$state)

  # subset data by column
  keep_cols = c(c("state", "date"), get_cols)
  sub_data = hosp_data[, keep_cols]

  # subset data by state
  state_index = sub_data$state == state
  sub_data = sub_data[state_index, ]

  # subset data by date
    # first convert dates from string/factor to date
  sub_data$date = as.Date(sub_data$date)
  # shift dates
  if (shift_dates) {
    sub_data$date = sub_data$date - 1
  }

  if (is.null(start_date)) {
    start_index = !logical(nrow(sub_data))
  } else {
    start_index = sub_data$date >= start_date
  }
  if (is.null(end_date)) {
    end_index = !logical(nrow(sub_data))
  } else {
    end_index = sub_data$date <= end_date
  }
  date_index = start_index & end_index
  sub_data = sub_data[date_index, ]

  # ensure that entries are sorted by date
  sub_data = sub_data[order(sub_data$date), ]

  return(sub_data)
}


#' @title Format HHS hospitalization data for fitting.
#'
#' @description After subsetting the admissions data to a single location, now format for
#' fitting.
#' @param state_data Dataframe. The output from get_HHS_state().
#' @param fit_col character. The name of the incidence columnn in 'state_data'
#' that should be used for model fitting.
#' @param state character.  The name of the location.
#' @param pop integer.  The location's population.
#' @param disease character.  The name of the disease.
#'
#' @return A list of lists.
#'     list(influenza=list(disease="influenza",
#'                         population=12345,
#'                         loc_name="profville",
#'                         inc_type="hosp_admits",
#'                         data=data.frame(date=dates,
#'                                       inc=incidence)))
#' @export
#'
#' @examples
#' format_hhs_state(state_data, fit_col, inc_type,loc_name, pop, disease)
#'
format_hhs_state <- function(state_data, fit_col, inc_type,
                             loc_name, pop, disease) {
  # initialize output list
  out_list = list()
  out_list[[disease]] = list(disease=disease, population=pop, loc_name=loc_name,
                             inc_type=inc_type)
  # extract incidence column
  inc_df = state_data[, c('date', fit_col)]
  names(inc_df) = c('date', 'inc')
  # add incidence data frame to output list
  out_list[[disease]][['data']] = inc_df

  return(out_list)
}


#' Test only
#'
#' @return make_config()
#'
test_import <- function() {
  make_config()
}



#' @title Example use of HHS data functions.
#'
#' @description This is a set of HHS function calls that demonstrate how to retrieve and
#' structure data for use with PROF fitting routines.  This function has
#' many inputs hard-coded (dates, population, etc) and should be used as a
#' blueprint rather than as an operational function.
#'
#' @param season integer start year of season 2021, 2022 or 2023 (in a few weeks)
#' @param state chr. Two-letter state/territory abbreviation.
#' @param fit_end Date.  Optional. Default is to fit all availablae data
#' for the season.  But if fit_date is provided all available data will
#' be downloaded but data will be fitted only until (and including) fit_date
#'
#' @return PROF data list.
#'
#' @examples
#' # download and format HHS hospitalization data for flu and COVID
#' PROF_data = hhs_data_ex(season = 2022, state="CA", fit_end = as.Date("2023-02-15"))
#' # plot the contents of a PROF data structure
#' plot_prof_data(prof_data=PROF_data)
#'
#' @import lubridate
#' @export
#'
hhs_data_ex <- function(season = NULL, state=NULL, fit_end = NULL) {
  # example use of HHS PROTECT data functions

  # download HHS hospitalizations file
  result = hhs_hosp_state_down(down_dir="~/Downloads")

  if (result$out_flag!=0) {
    stop("There was an error with the download.")
  }

  if(is.null(state)) {
    cat("\nState Abbbreviation Not Provided, Defaulting to CA\n")
    state = 'CA'
  }

  if (is.null(season)) {
    cat("\nSeason Not Provided, Defaulting to 2022-2023")
    season = 2022
  }

  if(!is.null(fit_end)) {
    fit_year = lubridate::year(fit_end)
    if (fit_year != season && fit_year != (season+1)) {
      stop("\nRequested fit_end is NOT consistent with selected season\n")
    }
  }

  # check that we are supporting the requested season

  supported_seasons = c(2021, 2022, 2023)

  if (!any(supported_seasons == season)) stop('\nRequested Season is Not Supported\n')

  # load the file
  hosp_data = load_HHS_csv(hhs_file=result$download_path)
  # hosp_data = load_HHS_csv(hhs_file="~/Dropbox/CSMB01/data/HHS_daily-hosp_state.csv")

  # --- Subset influenza admits data for Requested State ---
  # Set observed data subset dates

  start_date = as.Date(paste0(season,'-09-01'))
  end_date   = as.Date(paste0(season+1,'-06-01'))

  fit_start = as.Date(paste0(season,'-09-01'))

  if (season == 2023) {
    start_date = as.Date(paste0(season,'-08-01'))
    fit_start = as.Date(paste0(season,'-08-01'))
  }

  if (is.null(fit_end)) {
    fit_end = end_date #as.Date(paste0(season+1,"-02-15"))
  }

  keep_cols = c("previous_day_admission_influenza_confirmed",
                "previous_day_admission_influenza_confirmed_coverage",
                "previous_day_deaths_influenza",
                "previous_day_deaths_influenza_coverage")

  state_inf = get_HHS_state(hosp_data=hosp_data, get_cols=keep_cols,
                         state=state, start_date=start_date,
                         end_date=end_date)

  # format the data for fitting
  fit_col = "previous_day_admission_influenza_confirmed"
  inc_type = "hosp_admits"
  disease = "influenza"
  population = get_loc_pop(location=state)
  flu_data = format_hhs_state(state_data=state_inf, fit_col=fit_col, loc_name=state,
                              pop=population, disease=disease, inc_type=inc_type)

  # set fit data entry
  flu_data$influenza$data_fit = flu_data$influenza$data[
    flu_data$influenza$data$date >= fit_start &
      flu_data$influenza$data$date <= fit_end,
  ]


  # --- Repeat for COVID ---

  start_date = as.Date(paste0(season,'-10-15'))
  end_date   = as.Date(paste0(season+1,'-06-01'))

  if (season == 2023) {
    start_date = as.Date(paste0(season,'-07-01'))
    fit_start = as.Date(paste0(season,'-07-01'))
  }

  if (is.null(fit_end)) {
    fit_end = end_date #as.Date(paste0(season+1,"-02-15"))
  }

  keep_cols = c("previous_day_admission_adult_covid_confirmed",
                "previous_day_admission_adult_covid_confirmed_coverage",
                "previous_day_admission_pediatric_covid_confirmed",
                "previous_day_admission_pediatric_covid_confirmed_coverage",
                "deaths_covid", "deaths_covid_coverage")

  state_cov = get_HHS_state(hosp_data=hosp_data, get_cols=keep_cols,
                         state=state, start_date=start_date,
                         end_date=end_date)
  # combine adult and pediatric counts
  state_cov[["previous_day_admission_confirmed"]] =
    state_cov$previous_day_admission_adult_covid_confirmed +
    state_cov$previous_day_admission_pediatric_covid_confirmed

  # format the data for fitting
  fit_col = "previous_day_admission_confirmed"
  inc_type = "hosp_admits"
  disease = "covid19"
  population = get_loc_pop(location=state)
  cov_data = format_hhs_state(state_data=state_cov, fit_col=fit_col, loc_name=state,
                              pop=population, disease=disease, inc_type=inc_type)

  # set fit data entry
  cov_data$covid19$data_fit = cov_data$covid19$data[
    cov_data$covid19$data$date >= fit_start &
      cov_data$covid19$data$date <= fit_end,
  ]

  # --- Combine two diseases into a single data structure ---
  prof_data = list(covid19=cov_data$covid19, influenza=flu_data$influenza)

  return(prof_data)
}


#' @title Convert HHS hospitalization data to PROF format.
#'
#' @description This function converts HHS hospitalization data to the structure that
#' works with with PROF fitting routines.  This function retrieves data
#' for an entire season and expects the HHS data is already downloaded.
#' Before sending to fitting procedures, the output of this function
#' needs fitting date ranges (see PROF::hhs_set_fitdates()).
#'
#' @param hhs_path character. The local path to HHS data file.
#' @param season integer. The start year (YYYY) of season to model.
#' @param state character. 2-letter state abbreviation.
#'
#' @return PROF data structure (list of pathogen-data)
#' @export
#'
#'
hhs_2_PROF <- function(hhs_path=NULL, season = NULL, state=NULL) {

  if(is.null(state)) {
    cat("\nState Abbbreviation Not Provided, Defaulting to CA\n")
    state = 'CA'
  }

  if (is.null(season)) {
    cat("\nSeason Not Provided, Defaulting to 2022-2023")
    season = 2022
  }

  # check that we support the requested season
  supported_seasons = c(2021, 2022, 2023)

  if (!any(supported_seasons == season)) stop('\nRequested Season is Not Supported\n')

  if (is.null(hhs_path)) {
    stop("User must specify a path to the HHS data CSV file.",
         "  See PROF::hhs_hosp_state_down() to download current data file.")
  }
  # load the file
  hosp_data = load_HHS_csv(hhs_file=hhs_path)

  # --- Subset influenza admits data for Requested State ---
  # Set observed data subset dates

  start_date = as.Date(paste0(season,'-09-01'))
  end_date   = as.Date(paste0(season+1,'-06-01'))

  keep_cols = c("previous_day_admission_influenza_confirmed",
                "previous_day_admission_influenza_confirmed_coverage",
                "previous_day_deaths_influenza",
                "previous_day_deaths_influenza_coverage")

  state_inf = get_HHS_state(hosp_data=hosp_data, get_cols=keep_cols,
                            state=state, start_date=start_date,
                            end_date=end_date)

  # format the data for fitting
  fit_col = "previous_day_admission_influenza_confirmed"
  inc_type = "hosp_admits"
  disease = "influenza"
  population = get_loc_pop(location=state)
  flu_data = format_hhs_state(state_data=state_inf, fit_col=fit_col, loc_name=state,
                              pop=population, disease=disease, inc_type=inc_type)


  # --- Repeat for COVID ---
  start_date = as.Date(paste0(season,'-10-15'))
  end_date   = as.Date(paste0(season+1,'-06-01'))

  if (season == 2023) {
    start_date = as.Date(paste0(season,'-07-01'))
  }

  keep_cols = c("previous_day_admission_adult_covid_confirmed",
                "previous_day_admission_adult_covid_confirmed_coverage",
                "previous_day_admission_pediatric_covid_confirmed",
                "previous_day_admission_pediatric_covid_confirmed_coverage",
                "deaths_covid", "deaths_covid_coverage")

  state_cov = get_HHS_state(hosp_data=hosp_data, get_cols=keep_cols,
                            state=state, start_date=start_date,
                            end_date=end_date)
  # combine adult and pediatric counts
  state_cov[["previous_day_admission_confirmed"]] =
    state_cov$previous_day_admission_adult_covid_confirmed +
    state_cov$previous_day_admission_pediatric_covid_confirmed

  # format the data for fitting
  fit_col = "previous_day_admission_confirmed"
  inc_type = "hosp_admits"
  disease = "covid19"
  population = get_loc_pop(location=state)
  cov_data = format_hhs_state(state_data=state_cov, fit_col=fit_col, loc_name=state,
                              pop=population, disease=disease, inc_type=inc_type)

  # --- Combine two diseases into a single data structure ---
  prof_data = list(covid19=cov_data$covid19, influenza=flu_data$influenza)

  return(prof_data)
}


#' @title Add fit-data structures to PROF_data
#'
#' @description Fit-data structures explicitly state the data to be passed to the fitting
#' routine.  This allows the user to exclude data from the begining or end
#' of the season in the model fit.
#' @param prof_data list. Generally the output from PROF::hhs_2_PROF().
#' @param fit_start list. Each element is a Date or NULL with the lower limit
#'  (inclusive) of the data range to be fit.
#' @param fit_end Date. The upper limit (inclusive) of the data range to
#' be fit.
#'
#' @return PROF_data structure that now includes a $data_fit entry for
#' each pathogen.
#' @export
#'
#'
hhs_set_fitdates <- function(prof_data=NULL, fit_start=NULL, fit_end=NULL) {

  # determine all pathogens
  pathogens = names(prof_data)

  if (is.null(fit_start)) {
    fit_start = list()
    for (pathog in pathogens) fit_start[[pathog]] = NULL
  }

  for (pathog in pathogens) {
    if(!is.null(fit_end)) {
      if (!(fit_end<=max(prof_data[[pathog]]$data$date) &&
          fit_end>=min(prof_data[[pathog]]$data$date))) {
        stop("\nRequested fit_end is NOT consistent with selected season\n")
      }
      fit_end_use = fit_end
    } else {
      fit_end_use = max(max(prof_data[[pathog]]$data$date))
    }

    if (is.null(fit_start[[pathog]])) {
      fit_start_use = min(prof_data[[pathog]]$data$date)
    } else {
      fit_start_use = fit_start[[pathog]]
    }

    # set fit data entry
    prof_data[[pathog]]$data_fit = prof_data[[pathog]]$data[
      prof_data[[pathog]]$data$date >= fit_start_use &
        prof_data[[pathog]]$data$date <= fit_end_use,
    ]
  }

  return(prof_data)
}


#' @title Add fit-data-stat structures to PROF_data
#'
#' @description Fit-data-stat structures explicitly state the data to be passed to the statistical
#' fitting routine.  This allows the user to exclude data from the beginning or end
#' of the season in the model fit.
#' @param prof_data list. Generally the output from PROF::hhs_2_PROF().
#' @param fit_start list. Each element is a Date or NULL with the lower limit
#'  (inclusive) of the data range to be fit.
#' @param fit_end Date. The upper limit (inclusive) of the data range to
#' be fit.
#'
#' @return PROF_data structure that now includes a $data_fit_stat entry for
#' each pathogen.
#' @export
#'
#'
hhs_set_fitdates_stat <- function(prof_data=NULL, fit_start=NULL, fit_end=NULL) {

  # determine all pathogens
  pathogens = names(prof_data)

  if (is.null(fit_start)) {
    fit_start = list()
    for (pathog in pathogens) fit_start[[pathog]] = NULL
  }

  for (pathog in pathogens) {
    if(!is.null(fit_end)) {
      if (!(fit_end<=max(prof_data[[pathog]]$data$date) &&
            fit_end>=min(prof_data[[pathog]]$data$date))) {
        stop("\nRequested fit_end is NOT consistent with selected season\n")
      }
      fit_end_use = fit_end
    } else {
      fit_end_use = max(max(prof_data[[pathog]]$data$date))
    }

    if (is.null(fit_start[[pathog]])) {
      fit_start_use = min(prof_data[[pathog]]$data$date)
    } else {
      fit_start_use = fit_start[[pathog]]
    }

    # set fit data entry
    prof_data[[pathog]]$data_fit_stat = prof_data[[pathog]]$data[
      prof_data[[pathog]]$data$date >= fit_start_use &
        prof_data[[pathog]]$data$date <= fit_end_use,
    ]
  }

  return(prof_data)
}

#' @title Retrieve Poulation
#' @description Retrieve population of a U.S. state or territory.
#'
#' @param location character. Intended to be a two-letter abbreviation, but will
#' also attempt to match to location full names.
#'
#' @return integer. Population of the specified location.
#' @export
#'
#' @examples
#' get_loc_pop("CA")
#'
get_loc_pop <- function(location="US") {
  # load population file included in PROF package
  data("loc_pops")

  # match location to state abbreviation
  state_index = loc_pops$abbreviation == location
  if (sum(state_index)==0) {
    # attempt to match to full location names
    state_index = loc_pops$location_name == location

    if (sum(state_index)==0) {
      stop("Location provided to PROF::get_loc_pop() does not match any entries
           in the internal population dataset.")
    }
  }

  out_pop = loc_pops$population[state_index]

  return(out_pop)
}





