# PROF Command Line Tutorial

While we expect most users to interact with PROF using the Shiny app GUI, there are instances where using the package from the command line is preferred. For example, when sequentially fitting multiple locations.

The `prof_dev` sub-directory includes multiple example scripts with the `example.R` being the basic one. We suggest that you give it a try if you would like to learn to use PROF without its shiny GUI. You can download the `example.R` script from the link below: [Download the example.R script](files/example.R)

This example has been configured for fitting and forecasting influenza and COVID-19 data for any of the 50 states D.C. and Puerto Rico for the 2021-22, 2022-23, or 2023-24 seasons.

All available data is first downloaded and then the data for the requested location is retrieved and processed. You can fit the entire requested season or just part of it. Below we demonstrate both options.

We start by opening an R or Rstudio session and loading the PROF package:

``` r
> library(PROF)
```

We then download the HHS data using the provided `fetch_hhs_data` function

``` r
> result = fetch_hhs_data(down_dir="~/Downloads")
```

We can check to see if the download was successful:

``` r
> result$out_flag
```

The above should be zero.

We now select a state and a season

``` r
> state = "CA"

> season = 2023
```

and extract the data:

``` r
> prof_data = hhs_2_PROF(hhs_path=result\$download_path, season = season, state=state)
```

You can select other states or any of the two previous seasons (2021 and 2022).

The `prof_data` data structure should now be available and the data can be plotted to the screen:

``` r
> plot_prof_data(prof_data = prof_data)
```

Plots can also be saved to a file using:

``` r
> plot_prof_data(prof_data = prof_data, filename = '/path/to/filename')
```

Next we will add `fit data` structure to each pathogen - this is the data that will be fitted using a mechanistic compartmental model. NULL values for start/end dates mean set to start/end of the season data, and fit all available data:

``` r
> prof_data = hhs_set_fitdates(prof_data=prof_data, fit_start=NULL, fit_end=NULL)
```

To fit only part of the data use for example:

``` r
> prof_data = hhs_set_fitdates(prof_data=prof_data, fit_start=NULL, fit_end="2023-10-28")
```

You can also change the start date of the fit for each pathogen, for example:

``` r
> prof_data = hhs_set_fitdates(prof_data=prof_data, fit_start=c("2023-10-01",NULL), fit_end="2023-10-28")
```

will change the start fit for COVID-19 to October 1, 2023 but keep the start date for influenza to its default (start of the season) value.

Similarly,

``` r
> prof_data = hhs_set_fitdates(prof_data=prof_data, fit_start=c(NULL, "2023-10-01"), fit_end="2023-10-28")
```

will keep the default start date for COVID-19 and change it for influenza. Please note that whereas the start date can be set individually for each pathogen, there is a single end date for both pathogens.

If you would like to fit only part of the data we recommend using the interactive plots of the data for selecting the `fit_start` dates and `fit_end` date.

Please note that whereas each pathogen has its own `fit_start` date, the end date for the fitting is always the same for both pathogen.

Next we load the parameters for the models (for more details [Download the ex_par_list.R script](files/ex_par_list.R)):

``` r
> par_list = init_par_list(diseases=c("covid19", "influenza"), models=c("seirh", "sirh"))
```

The above call tells PROF that we would like to fit SEIRH/SIRH models to the COVID-19/influenza data.

To fit an SEIRH model for both COVID-19 and influenza use:

``` r
> par_list = init_par_list(diseases=c("covid19", "influenza"), models=c("seirh", "seirh"))
```

We can now sequentially fit both pathogens using the compartmental models we selected:

``` r
> fit_list = fit_data(prof_data = prof_data, par_list = par_list)
```

You can now sit and relax for 5-10 minutes

To save the results of the fit (posterior distribution and initial state) use:

``` r
> saveRDS(fit_list, filename = '/path/to/filename.rds')
```

To plot the results of the fit to the screen use:

``` r
> fit_traj = plot_fit(prof_data = prof_data, par_list = par_list, fit_list = fit_list)
```

The plotting routine returns a list with the following elements: fit_traj - a list for each disease containing: model fit mechanistic trajectories, dates, and reported incidence pl - a list of ggplot2 objects one for each disease for the mechanistic plots

To save the plot to a file use:

``` r
> fit_traj = plot_fit(prof_data = prof_data, par_list = par_list, fit_list = fit_list, filename = '/path/to/filename')
> plot_fit_list$arrange_plot
```

The above call returns a list with the `arrange_plot` being an interactive plotly object of the statistical fit plots.

To use the posterior distributions of the compartmental fits to create individual forecasts and a combined burden forecast use:

``` r
> forecast_traj = plot_forecast(prof_data = prof_data, par_list = par_list, fit_list = fit_list)
> forecast_traj$arrange_plot
```

PROF uses our error correlation procedure to estimate the combined burden. Two estimates are provided:

1.  Random Estimate (bottom left panel): This estimate uses an error correlation value of zero.
2.  Sorted Estimate (bottom right panel): This estimate uses a default error correlation value of one, with a range between zero and one. The user can modify the default value for the sorted option.

The random estimate is always generated, but you can customize the sorted estimate using this command:

``` r
> forecast_traj = plot_forecast(prof_data = prof_data, par_list = par_list, fit_list = fit_list, err_cor = 0.7)
> forecast_traj$arrange_plot
```

To also save the plot to a file use:

``` r
> forecast_traj = plot_forecast(prof_data = prof_data, par_list = par_list, fit_list = fit_list, err_cor = 0.7, filename = '/path/to/filename')
```

PROF can also be used to fit and forecast using a fast baseline statistical model. The procedure follows the steps we have taken for the compartmental mechanistic model.

First, we add `fit-stat data` structure to each pathogen - this is the data that will be fitted using a baseline statistical model. NULL values for start/end dates mean set to start/end of the season data, and fit all available data:

``` r
> prof_data = hhs_set_fitdates_stat(prof_data=prof_data, fit_start=c(NULL,NULL), fit_end=NULL)
```

Note that here too the start date for fitting can be different for each pathogen but the end date for fitting is the same.

To fit and plot the results we use:

``` r
> stat_fit_list = plot_stat_fit(prof_data = prof_data, ntarj = 2e4, filename = NULL)
```

Here we have set the number of trajectories to 2,000 (default is 1,000). To save the plots to a file use:

``` r
> stat_fit_list = plot_stat_fit(prof_data = prof_data, ntarj = 2e4, filename = 'path/to/filename')
```

To use the baseline statistical model to create a 28 day forward forecast and the two estimates for the combined burden (with an error correlation value of 0.5 for example) use:

``` r
> stat_forecast_list = plot_stat_forecast(prof_data = prof_data, nfrcst = 28, err_cor = 0.5)
> stat_forecast_list$arrange_plot
```

For both the mechanistic and statistical options, the number of forecast horizons is determined by the nfrcst parameter, with the unit of time always set to days (even if the incidence data cadence is in weeks). By default, this is set to 28 days. For weekly data, nfrcst must be a multiple of seven (e.g., 7, 14, 21, etc.).

Finally, we demonstrate how to use PROF to fit only one pathogen.

If for example you would like to only fit the 'covid19' data you should follow these steps:

Load an initial guess for the parameters and set the model only for 'covid19'

``` r
> par_list = init_par_list(diseases=c("covid19"), models=c("seirh"))
```

Fit only the covid19 data using:

``` r
> fit_list = fit_data(prof_data = prof_data['covid19'], par_list = par_list)
```

Plot the single pathogen fit results:

``` r
> fit_traj = plot_fit(prof_data = prof_data['covid19'], par_list = par_list, fit_list = fit_list)
> fit_traj$arrange_plot
```

Use the posterior distribution of the single pathogen fit to perform and plot the forecast:

``` r
> forecast_traj = plot_forecast(prof_data = prof_data['covid19'], par_list = par_list, fit_list = fit_list)
> forecast_traj$arrange_plot
```

Since only one pathogen was modeled there is no estimate for a combined burden.

Similarly, to fit a baseline statistical model only to covid19 use:

``` r
> stat_fit_list = plot_stat_fit(prof_data = prof_data['covid19'])
> stat_fit_list$arrange_plot
```

And for the forecast:

``` r
> stat_forecast_list = plot_stat_forecast(prof_data = prof_data['covid19'], nfrcst = 28)
> stat_forecast_list$arrange_ploy
```

To fit and forecast only influenza use the same logic in all the calls shown above replacing 'covid19' with 'influenza' and using an 'sirh' compartmental model.
