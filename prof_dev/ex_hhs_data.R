
# example use of HHS PROTECT data functions
library(PROF)

# download HHS hospitalizations file
result = fetch_hhs_data(down_dir="~/Downloads")

if (result$out_flag!=0) {
  stop("There was an error with the download.")
}

# load the file
hosp_data = load_HHS_csv(hhs_file=result$download_path)
# hosp_data = load_HHS_csv(hhs_file="~/Dropbox/CSMB01/data/HHS_daily-hosp_state.csv")

# --- Subset influenza admits data for California ---
state = "CA"
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
state = "CA"
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


