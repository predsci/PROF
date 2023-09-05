

#' Download daily state-level HHS PROTECT hospitalization admission data
#' to a CSV.
#'
#' @param down_dir character string. The directory path to download to.
#' @param down_filename character string. The filename to download to.
#'
#' @return an integer. 0 for success and non-zero for failure.
#' @export
#'
#' @examples
hhs_hosp_state_down <- function(down_dir="~/", down_filename=NULL) {


  api_url = "https://healthdata.gov/api/views/g62h-syeh/rows.csv?accessType=DOWNLOAD"

  if (is.null(down_filename)) {
    save_filename_base = "HHS_daily-hosp_state"
    today_date = Sys.Date()
    down_filename = paste0(save_filename_base, ".csv")
  }

  # download the new file (overwriting the previous current file)
  download_path = file.path(down_dir, down_filename)
  cat("Attempting download at time: ", format(Sys.time()), "\n", sep="")
  out_flag = download.file(url=api_url, destfile=download_path)

  return(list(download_path=download_path, out_flag=out_flag))
}


#' Open the HHS Protect CSV.
#'
#' Read the file with stringsAsFactors=TRUE, and return a dataframe.
#' @param hhs_file Character string. Path to CSV file.
#'
#' @return dataframe of all HHS PROTECT columns
#' @export
#'
#' @examples
load_HHS_csv <- function(hhs_file=NULL) {
  # open raw data file
  hosp_data = read.csv(file=hhs_file, stringsAsFactors=T)
  return(hosp_data)
}


#' Subset HHS PROTECT hospitalizations by state, metric, and dates.
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


#' Format HHS hospitalization data for fitting.
#'
#' After subsetting the admissions data to a single location, now format for
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
#' @return
#'
#' @examples
test_import <- function() {
  make_config()
}



#' Example use of HHS data functions.
#' 
#' This is a set of HHS function calls that demonstrate how to retrieve and 
#' structure data for use with PROF fitting routines.  This function has 
#' many inputs hard-coded (dates, population, etc) and should be used as a 
#' blueprint rather than as an operational function.
#' @param state chr. Two-letter state/territory abbreviation.
#'
#' @return PROF data list.
#' @export
#'
#' @examples
#' # download and format HHS hospitalization data for flu and COVID
#' PROF_data = hhs_data_ex(state="CA")
#' # plot the contents of a PROF data structure
#' plot_prof_data(prof_data=PROF_data)
hhs_data_ex <- function(state="CA") {
  # example use of HHS PROTECT data functions
  
  # download HHS hospitalizations file
  result = hhs_hosp_state_down(down_dir="~/Downloads")
  
  if (result$out_flag!=0) {
    stop("There was an error with the download.")
  }
  
  # load the file
  hosp_data = load_HHS_csv(hhs_file=result$download_path)
  # hosp_data = load_HHS_csv(hhs_file="~/Dropbox/CSMB01/data/HHS_daily-hosp_state.csv")
  
  # --- Subset influenza admits data for California ---
  # Set observed data subset dates
  start_date = as.Date("2022-09-01")
  end_date = as.Date("2023-06-01") #Sys.Date()
  # Set data fitting subset dates
  fit_start = as.Date("2022-09-01")
  fit_end = as.Date("2023-02-15")
  
  keep_cols = c("previous_day_admission_influenza_confirmed",
                "previous_day_admission_influenza_confirmed_coverage",
                "previous_day_deaths_influenza",
                "previous_day_deaths_influenza_coverage")
  
  CA_inf = get_HHS_state(hosp_data=hosp_data, get_cols=keep_cols,
                         state=state, start_date=start_date,
                         end_date=end_date)
  
  # format the data for fitting
  fit_col = "previous_day_admission_influenza_confirmed"
  inc_type = "hosp_admits"
  disease = "influenza"
  population = 39144818                   # write function to look-up state pops
  flu_data = format_hhs_state(state_data=CA_inf, fit_col=fit_col, loc_name=state,
                              pop=population, disease=disease, inc_type=inc_type)
  
  # set fit data entry
  flu_data$influenza$data_fit = flu_data$influenza$data[
    flu_data$influenza$data$date >= fit_start &
      flu_data$influenza$data$date <= fit_end,
  ]
  
  
  # --- Repeat for COVID ---
  start_date = as.Date("2022-10-01")
  end_date = as.Date("2023-06-01") #Sys.Date()
  keep_cols = c("previous_day_admission_adult_covid_confirmed",
                "previous_day_admission_adult_covid_confirmed_coverage",
                "previous_day_admission_pediatric_covid_confirmed",
                "previous_day_admission_pediatric_covid_confirmed_coverage",
                "deaths_covid", "deaths_covid_coverage")
  
  CA_cov = get_HHS_state(hosp_data=hosp_data, get_cols=keep_cols,
                         state=state, start_date=start_date,
                         end_date=end_date)
  # combine adult and pediatric counts
  CA_cov[["previous_day_admission_confirmed"]] =
    CA_cov$previous_day_admission_adult_covid_confirmed +
    CA_cov$previous_day_admission_pediatric_covid_confirmed
  
  # format the data for fitting
  fit_col = "previous_day_admission_confirmed"
  inc_type = "hosp_admits"
  disease = "covid19"
  population = 39144818                   # write function to look-up state pops
  cov_data = format_hhs_state(state_data=CA_cov, fit_col=fit_col, loc_name=state,
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
