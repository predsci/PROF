# Setup

We have setup an example.R script and suggest that you give it a try after installing the package.

This example has been configured for fitting and forecasting influenza and COVID-19 data for any of the 50 states D.C. and Puerto Rico for either the 2021-2022 or 2022-2023 seasons.

All available data is first downloaded and then the data for the requested location is retrieved and processed. You can fit the entire requested season or just part of it.  Below we demonstrate both options.  

All available data for the season is downloaded fitting uses data only until February 15, 2023.   You can then compare the forecast to the available data.

We start by opening an R or RStudio session and loading the PROF package:

>\> library(PROF)

We then use the provided 'hhs_data_ex' function to download the HHS hospitalization file, subset to a state and format the data for both COVID-19 and influenza.
We also define the season and the end date for fitting.

>\> prof_data = hhs_data_ex(season = 2022, state="CA", fit_end = as.Date("2023-02-15"))

To fit the entire season use:
>\> prof_data = hhs_data_ex(season = 2022, state="CA")

To fit the entire  2021-2022 season for Washington state use:

>\> prof_data = hhs_data_ex(season = 2021, state="WA")

and to fit part of this season to data for the state of Texas use:
>\> prof_data = hhs_data_ex(season = 2021, state="TX", fit_end = as.Date("2022-04-15"))

When the download is complete we plot the data to the screen:

>\> plot_prof_data(prof_data = prof_data)

To save the plot to a file use:

>\> plot_prof_data(prof_data = prof_data, filename = '/path/to/filename')

Next we load the parameters for the models (for more details see the R/ex_par_list.R script):

>\> par_list = init_par_list(diseases=c("covid19", "influenza"), models=c("seirh", "sirh"))

The above call tells PROF that we would like to fit SEIRH/SIRH models to the COVID-19/influenza data. 

To fit an SEIRH model for both COVID-19 and influenza use:

>\> par_list = init_par_list(diseases=c("covid19", "influenza"), models=c("seirh", "seirh"))

We can now sequentially fit both pathogens:

>\> fit_list <- fit_data(prof_data = prof_data, par_list = par_list)

You can now sit and relax for 10-15 minutes

To save the results of the fit (posterior distribution and initial state) use:

>\> saveRDS(fit_list, filename = '/path/to/filename.rds')

To plot the results of the fit to the screen use:

>\> fit_traj <- plot_fit(prof_data = prof_data, par_list = par_list, fit_list = fit_list)

The plotting routine returns a list with two elements ('covid19' and 'influenza') each element is a list with the trajectories used to create the plots, the date array
and the reported incidence array.

To save the plot to a file use:

>\> fit_traj <- plot_fit(prof_data = prof_data, par_list = par_list, fit_list = fit_list, filename = '/path/to/filename')


To use the posterior distributions of the fits to create individual forecasts and  a combined burden forecast use:

>\> forecast_traj <- plot_forecast(prof_data = prof_data, par_list = par_list, fit_list = fit_list)

Please note that we currently provide two versions of the combined forecast: random (bottom left panel), and ordered (bottom right panel).

The plotting routine returns a list with four elements ('covid19', 'influenza', 'random', and 'ordered'). Random and Ordered are the combined burden calculated with random and ordered selection of trajectories, respectively. Each element is a list with the trajectories used to create the plots, the date array and the reported incidence array.

To also save the plot to a file use:

>\> forecast_traj <- plot_forecast(prof_data = prof_data, par_list = par_list, fit_list = fit_list, filename = '/path/to/filename')

