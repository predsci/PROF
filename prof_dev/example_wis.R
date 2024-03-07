# example use of PROF for evaluating the accuracy of forecasts using the WIS score
# We will use data for CA, 2023-24 season

library(PROF)
library(plotly)
library(deSolve)

# download HHS Hospitalization file

result = fetch_hhs_data(down_dir="~/Downloads")

# set state and season and extract data
state = "CA"
season = 2023
prof_data = hhs_2_PROF(hhs_path=result$download_path, season = season, state=state)

# We will plot the data and hover over it to see the end date of the data
# we can also query the data structure as shown below

plot_prof_data(prof_data = prof_data)

# to seewhen the data ends you can use:
tail(prof_data[['covid19']]$data)
tail(prof_data[['influenza']]$data)

# suppose the last date is '2024-03-02'
# We will fit the two patogens until 28 days before this date and evaulate the accuracy
# of the fits by comparing the 28-day forward forecast to the data for these 28 days
last_data_date = as.Date('2024-03-02')

fit_end_date = last_data_date - 28


# to fit the data from the start date until fit _end_date:

prof_data = hhs_set_fitdates(prof_data=prof_data,
                             fit_start=c(NULL, NULL),
                             fit_end=c('covid19'=fit_end_date,
                                       'influenza'=fit_end_date))

# We can view the fitting and forecasting time windows by re-plotting the data.
# The data we are NOT fitting will have a higher transperancy

plot_prof_data(prof_data = prof_data)

# to load the parameters for the models use:
# for more details see the ex_par_list.R script

par_list = init_par_list(diseases=c("covid19", "influenza"),
                         models=c("seirh", "sirh"))


# to load the parameters for the models use:
# for more details see the ex_par_list.R script

par_list = init_par_list(diseases=c("covid19", "influenza"),
                         models=c("seirh", "sirh"))

# to fit both pathogens use:
# Here we use a 3-value model for the FOI for COVID19 and a 2-value for influenza
# you can now seat and relax for 10-15 minutes

fit_list <- fit_data(prof_data = prof_data, par_list = par_list, nb_vec=c(3,2))

# We will now plot the results to the screen

plot_fit_list <- plot_fit(prof_data = prof_data, par_list = par_list, fit_list = fit_list)

# We now fit the same time window using the statistical model

prof_data = hhs_set_fitdates_stat(prof_data=prof_data,
                                  fit_start=c(NULL, NULL),
                                  fit_end=c('covid19'=fit_end_date,
                                            'influenza'=fit_end_date))
stat_fit_list <- plot_stat_fit(prof_data = prof_data, ntraj = 1e4, filename = NULL)

# Mechanistic forecast 28 days forward, the function will also score our mechanistic forecast

forecast_list <- plot_forecast(prof_data = prof_data, par_list = par_list, fit_list = fit_list,
                               nfrcst = 28)

# Baseline Statistical forecast 28 days forward, the function will also score our mechanistic forecast

stat_forecast_list <- plot_stat_forecast(prof_data = prof_data, nfrcst = 28)

# retrieve teh WIS information for mechanistic forecast
wis_df_mech = forecast_list$wis_df

# retrieve teh WIS information for statisical forecast
wis_df_stat = stat_forecast_list$wis_df

# create a list with all mechanistic and statistical WIS information
wis_data = list(mech=wis_df_mech, stat = wis_df_stat)

# Call plotting routine. To save plot to a file provide a filename

wis_df = plot_wis(wis_data = wis_data, loc = state, filename = NULL)



