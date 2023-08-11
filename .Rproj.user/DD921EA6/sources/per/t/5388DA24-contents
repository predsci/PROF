
# example use of PROF
# Fitting and forecasting 2022-23 covid19 and influenza data for  CA 2022-23

library(PROF)

# set path to location of example.R script

setwd("~/Dropbox/CSMB03/code/PROF/prof_dev")

# use the provided 'ex_hhs_data.R' script to download the HHS hospitlization
# file, subset to CA and format the data for both covid19 and influenza

source("ex_hhs_data.R")

# The 'prof_data' data structure should now be available and the data can
# be plotted

# To plot the data to a screen use:

plot_prof_data(prof_data = prof_data)

# To plot the data to the screen and save to a file use
# plot_prof_data(prof_data = prof_data, filename = '/path/to/filename')

# to load the parameters for the models use:
# for more details see the ex_par_list.R script

par_list = init_par_list(diseases=c("covid19", "influenza"),
                          models=c("seirh", "sirh"))

# to fit both pathogens use:

fit_list <- fit_data(prof_data = prof_data, par_list = par_list)

# you can now seat and relax for 10-15 minutes

# to plot the results of the fit to the screen

plot_fit(prof_data = prof_data, par_list = par_list, fit_list = fit_list)

# to also save to a file use:
#  plot_fit(prof_data = prof_data, par_list = par_list,
# fit_list = fit_list, filename = '/path/to/filename')


# to use the posterior distributions of the fits to create individual forecasts
# and combined burden forecasts use:

plot_fprecast(prof_data = prof_data, par_list = par_list, fit_list = fit_list)

# please note that we currently provide two versions of the combined forecast:
# random (bottom left panel), and ordered (bottom right panel)

# to also save the plot to a file use:
#  plot_forecast(prof_data = prof_data, par_list = par_list,
# fit_list = fit_list, filename = '/path/to/filename')

###############

