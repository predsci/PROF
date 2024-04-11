
# example use of PROF for weekly data
# Fitting and forecasting weekly user data
# covid19 and influenza

library(PROF)
library(plotly)
library(deSolve)
library(lubridate)

#path to a file with daily data

filepath = "~/Downloads/example_inc.csv"

# read daily data
raw_csv <- read.csv(file=filepath, stringsAsFactors=T)

# ensure date is in format "%Y-%m-%d"

raw_csv$date <- ymd(raw_csv$date)

# Add EW column to DF

raw_csv$ew <- as.numeric(epitools::as.week(raw_csv$date)$week)

# Start conversion daily to weekly (simple accounting for incomplete weeks)

df=list()
for (disease_level in levels(raw_csv$disease)) {
  sub_csv = subset(raw_csv, disease == disease_level)
  ew_arr = unique(sub_csv$ew)

  df[[disease_level]] = data.frame(date = rep(as.Date(sub_csv[1,'date']),length(ew_arr)), disease = sub_csv[1:length(ew_arr),'disease'],
                                   metric = sub_csv[1:length(ew_arr),'metric'],
                                   value = rep(0, length(ew_arr)), ew=ew_arr)
  icount=0
  for (iweek in ew_arr) {
    tmp = subset(sub_csv, ew == iweek)
    weekly_value = sum(tmp[,'value'])
    frac = 7/nrow(tmp)
    icount=icount+1
    df[[disease_level]][icount,'value'] = round(weekly_value * frac)
    df[[disease_level]][icount,'date']  = max(tmp[,'date'])
  }

}

long_df <- dplyr::bind_rows(df)

data_weekly = long_df[,c('date','disease', 'metric', 'value')]

# end of conversion to weekly

# save the weekly data

filepath_weekly = "~/Downloads/example_inc_weekly.csv"

write.csv(data_weekly, file = filepath_weekly, row.names = FALSE)

# load weekly data
prof_data=csv_to_prof(filepath = filepath_weekly, population = 4e7, location = "CA-WEEKLY")

# To plot the data to a screen use:

plot_prof_data(prof_data = prof_data)


# to fit from the first data point shown use this default call:
prof_data = hhs_set_fitdates(prof_data=prof_data,
                             fit_start=NULL, fit_end=NULL)

# To plot the data to the screen and save to a file use
# plot_prof_data(prof_data = prof_data, filename = '/path/to/filename')

# to load the parameters for the models use:
# for more details see the ex_par_list.R script

par_list = init_par_list(diseases=c("covid19", "influenza"),
                         models=c("seirh", "sirh"))

# Fit models to weekly COVID-19 and Influenza data

fit_list <- fit_data(prof_data = prof_data, par_list = par_list, nb_vec=c(3,2))

# Sample from Posterior distribution and create fit plots
plot_fit_list <- plot_fit(prof_data = prof_data, par_list = par_list, fit_list = fit_list)

# Display interactive fit plots
plot_fit_list$arrange_plot

# To use the posterior distributions of the compartmental fits to create individual forecasts
# `nfrcst'` days forward and combined burden forecasts use:

forecast_list <- plot_forecast(prof_data = prof_data, par_list = par_list, fit_list = fit_list,
                               nfrcst = 28)

# Please note that although your data may be weekly the code expects the forecast horizon to be
# in days and in multiples of seven

# to plot the forecast (interactive plots)

forecast_list$ararnge_plot

# statistical forecast

# We can also fit a baseline statistical model to the data.
# We start by adding the fit-stat data structure to each pathogen - this is the data
# that will be fitted with a simple baseline statistical model


prof_data = hhs_set_fitdates_stat(prof_data=prof_data, fit_start=NULL, fit_end=NULL)

# Running and plotting the fitting of a simple statistical baseline model will only take a
# few seconds:

stat_fit_list <- plot_stat_fit(prof_data = prof_data, ntraj = 1e4, filename = NULL)

# For an interactive plot of the baseline sttaistical model

stat_fit_list$arrange_plot


# to use a baseline statistical model and create individual forecasts 28 days forward
# and combined burden forecasts use:

stat_forecast_list <- plot_stat_forecast(prof_data = prof_data, nfrcst = 28)

# Again: Please note that although your data may be weekly the code expects the forecast horizon to be
# in days and in multiples of seven

stat_forecast_list$arrange_plot


