# Example of how to use the data functions separately

library(PROF)
# source("~/Dropbox/GitReps/PROF/R/data_download_format.R")

season = 2022
state= "CA"
fit_start = NULL
fit_end = as.Date("2023-01-31")


# download HHS hospitalizations file
result = hhs_hosp_state_down(down_dir="~/Downloads")

if (result$out_flag!=0) {
  stop("There was an error with the download.")
}

# extract state and structure for PROF
hhs_path = result$download_path
prof_data = hhs_2_PROF(hhs_path=hhs_path, season = season, state=state)


# add fit data structure to each pathogen
# NULL values for start/end dates mean to set to start/end of season data
prof_data = hhs_set_fitdates(prof_data=prof_data, fit_start=fit_start, 
                             fit_end=fit_end)


