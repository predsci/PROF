# example use of PROF
# Fitting and forecasting 2023-24 covid19 and influenza seasons

library(PROF)
library(plotly)
library(deSolve)

# download the most recent HHS Hospitalization file

result = fetch_hhs_data(down_dir="~/Downloads")

# set state and season and extract data
state = "CA"
season = 2023
prof_data = hhs_2_PROF(hhs_path=result$download_path, season = season, state=state)

disease = 'influenza'

# The 'prof_data' data structure should now be available and the data can
# be plotted

# To plot the data to a screen use:

plot_prof_data(prof_data = prof_data)

prof_data = hhs_set_fitdates(prof_data=prof_data,
                             fit_start=NULL, fit_end=NULL)

par_list = init_par_list(diseases=c("influenza"),
                         models=c("sirh"))

#

fit_list <- fit_data(prof_data = prof_data['influenza'], par_list = par_list, nb_vec=c(3))

# to plot the results of the fit to the screen use these two calls:

plot_fit_list <- plot_fit(prof_data = prof_data['influenza'], par_list = par_list, fit_list = fit_list)

plot_fit_list$arrange_plot

prof_data = hhs_set_fitdates_stat(prof_data=prof_data['influenza'], fit_start=NULL, fit_end=NULL)

stat_fit_list <- plot_stat_fit(prof_data = prof_data['influenza'], ntraj = 1e4, filename = NULL)

forecast_list <- plot_forecast(prof_data = prof_data['influenza'], par_list = par_list, fit_list = fit_list, nfrcst = 28)

forecast_list$arrange_plot

forecast_stat_list <- plot_stat_forecast(prof_data = prof_data['influenza'], nfrcst = 28)

forecast_stat_list$arrange_plot

forecast_mix_list <- plot_mixed_forecast(prof_data = prof_data, forecast_list = forecast_list, forecast_stat_list = forecast_stat_list)

