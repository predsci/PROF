# Setup

We have setup an example.R script and suggest that you give it a try after installing the package.

This example has been configured for fitting and forecasting influenza and COVID-19 data for any of the 50 states D.C. and Puerto Rico for the 2021-22, 2022-23, or 2023-24 seasons.

All available data is first downloaded and then the data for the requested location is retrieved and processed. You can fit the entire requested season or just part of it.  Below we demonstrate both options.  



We start by opening an R or RStudio session and loading the PROF package:

>\> library(PROF)

We then download the HHS data using the provided 'hhs_hosp_state_down' function

>\> result = hhs_hosp_state_down(down_dir="~/Downloads")

We can check to see if the download was successful:

>\> result$out_flag 

The above should be zero.

We know select a state and a season

>\> state = "CA"

>\> season = 2023

and extract the data:

>\> prof_data = hhs_2_PROF(hhs_path=result$download_path, season = season, state=state)

You can select other states or any of the two previous seasons (2021 and 2022).

The 'prof_data' data structure should now be available and the data can be plotted to the screen:

>\> plot_prof_data(prof_data = prof_data)

Plots can also be saved to a file using:

>\> plot_prof_data(prof_data = prof_data, filename = '/path/to/filename')

Next we will add 'fit data' structure to each pathogen - this is the data that will be fitted
NULL values for start/end dates mean set to start/end of the season data, and fit all avialable data:

prof_data = hhs_set_fitdates(prof_data=prof_data, fit_start=NULL, fit_end=NULL)

To fit only part of the data use for example:

prof_data = hhs_set_fitdates(prof_data=prof_data, fit_start=NULL, fit_end="2023-10-28")

If you would like to fit only part of the data we recommend using the interactive plots of the data for selecting  the 'fit_end' date.

Next we load the parameters for the models (for more details see the R/ex_par_list.R script):

>\> par_list = init_par_list(diseases=c("covid19", "influenza"), models=c("seirh", "sirh"))

The above call tells PROF that we would like to fit SEIRH/SIRH models to the COVID-19/influenza data. 

To fit an SEIRH model for both COVID-19 and influenza use:

>\> par_list = init_par_list(diseases=c("covid19", "influenza"), models=c("seirh", "seirh"))

We can now sequentially fit both pathogens using the compartmental models we selected:

>\> fit_list <- fit_data(prof_data = prof_data, par_list = par_list)

You can now sit and relax for 10-15 minutes

To save the results of the fit (posterior distribution and initial state) use:

>\> saveRDS(fit_list, filename = '/path/to/filename.rds')

To plot the results of the fit to the screen use:

>\> fit_traj <- plot_fit(prof_data = prof_data, par_list = par_list, fit_list = fit_list)

Please note that by default this routine also plots the results of fitting a baseline statistical model to the data.  Early in the season this may provide a reasonable fit and
forecast.

The plotting routine returns a list with the following elements:
 fit_traj - a list for each disease containing: model fit mechanistic trajectories, dates, and reported incidence
pl - a list of ggplot2 objects one for each disease for the mechanistic plots


To save the plot to a file use:

>\> fit_traj <- plot_fit(prof_data = prof_data, par_list = par_list, fit_list = fit_list, filename = '/path/to/filename')


To plot the results of fitting a baseline statistical mode to each pathogen use:
>\> stat_fit_list <- plot_stat_fit(prof_data = prof_data)

The above call returns a list with the following elements:

stat_fit_traj - a list for each disease containing: baseline statistical fit trajectories, dates, and reported incidence
pl_stat - a list of ggplot2 objects one for each disease for the statistical plots

To use the posterior distributions of the fits to create individual forecasts and  a combined burden forecast use:

>\> forecast_traj <- plot_forecast(prof_data = prof_data, par_list = par_list, fit_list = fit_list)

Please note that we currently provide two versions of the combined forecast: random (bottom left panel), and ordered (bottom right panel).

The plotting routine returns a list with four elements ('covid19', 'influenza', 'random', and 'ordered'). Random and Ordered are the combined burden calculated with random and ordered selection of trajectories, respectively. Each element is a list with the trajectories used to create the plots, the date array and the reported incidence array.

To also save the plot to a file use:

>\> forecast_traj <- plot_forecast(prof_data = prof_data, par_list = par_list, fit_list = fit_list, filename = '/path/to/filename')

To use the baseline statistical model to create a 42 day forward forecast and the two estimates for the combined burden use:
>\> stat_forecast_list <- plot_stat_forecast(prof_data = prof_data, nfrcst = 42)

For both the mechanistic and statistical options the number of forecast horizons is set by the parameter 'nfrcst' above. By default it is set to 35.  The cadence (i.e., units)
of this parameter are assumed to be the same as the same as that of the incidence data.



