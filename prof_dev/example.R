
# example use of PROF
# Fitting and forecasting 2022-23 covid19 and influenza data for  CA 2022-23

library(PROF)

# set path to location of example.R script

setwd("~/Dropbox/CSMB03/michal/PROF/prof_dev")

# download HHS Hospitalization file

result = hhs_hosp_state_down(down_dir="~/Downloads")

# set state and season and extract data
state = "CA"
season = 2023
prof_data = hhs_2_PROF(hhs_path=result$download_path, season = season, state=state)


# The 'prof_data' data structure should now be available and the data can
# be plotted

# To plot the data to a screen use:

plot_prof_data(prof_data = prof_data)

# add fit data structure to each pathogen - this is the data that will
# be fitted
# NULL values for start/end dates mean set to start/end of the season data

prof_data = hhs_set_fitdates(prof_data=prof_data, fit_start=NULL, fit_end=NULL)

# To plot the data to the screen and save to a file use
# plot_prof_data(prof_data = prof_data, filename = '/path/to/filename')

# to load the parameters for the models use:
# for more details see the ex_par_list.R script

par_list = init_par_list(diseases=c("covid19", "influenza"),
                          models=c("seirh", "sirh"))

# to fit both pathogens use:

fit_list <- fit_data(prof_data = prof_data, par_list = par_list)

# you can now seat and relax for 10-15 minutes

# to save the results of the fit (posterior distribution and initial state)

saveRDS(fit_list, filename = '/path/to/filename.rds')

# to plot the results of the fit to the screen. This function will also create and
# display the results of a statistical baseline model

plot_fit_list <- plot_fit(prof_data = prof_data, par_list = par_list, fit_list = fit_list)


# The plotting routine returns a list with the following elements
# fit_traj - a list for each disease containing: model fit mechanistic trajectories,
# dates, and reported incidence
# stat_traj - a list for each disease containing: baseline statistical trajectories,
# dates and reported incidence
# pl - a list of ggplot2 objects one for each disease for the mechanistic plots
# pl_stat - a list of ggplot2 objects one for each disease for the statistical plots

# to save the plots to a file use:
#  plot_fit_list <- plot_fit(prof_data = prof_data, par_list = par_list,
# fit_list = fit_list, filename = '/path/to/filename')


# To plot the results if fitting a baseline statistical model to the data use:

stat_fit_list <- plot_stat_fit(prof_data = prof_data)

# to use the posterior distributions of the fits to create individual forecasts 35 days forward
# and combined burden forecasts use:

forecast_list <- plot_forecast(prof_data = prof_data, par_list = par_list, fit_list = fit_list,
                               nfrcst = 35)


# please note that we currently provide two versions of the combined forecast:
# random (bottom left panel), and ordered (bottom right panel)

# the plotting routine returns a list with four elements ('covid19', 'influenza', 'random', and
# 'ordered'). Random and Ordered are the combined burden calculated with random and ordered
# selection of trajectories, respectively.
# Each element is a list with the trajectories used to create the plots, the date array
# and the reported incidence array
#

# to also save the plot to a file use:
#  plot_forecast(prof_data = prof_data, par_list = par_list,
# fit_list = fit_list, filename = '/path/to/filename')


# to use a baseline statistical model  create individual forecasts 35 days forward
# and combined burden forecasts use:

stat_forecast_list <- plot_stat_forecast(prof_data = prof_data, nfrcst = 35)

# For the combined burden of the baseline statistical model we offer the same two
# options (random and sorted)

# Add example of combining mechanistic for COVID-19 and statistical for influenza
###############

