
#
#' Estimate the initial number of infections
#'
#' Use the EpiEstim package to estimate initial number of infections
#' @param inc time series of hospitalization incidence
#' @param mydisease disease name
#' @return integer estimate for initial number of cases
#' @keywords internal
#'
est_I0 <- function(inc, mydisease) {

  ind = grep(mydisease, 'covid19', ignore.case = TRUE)

  # calculate 1-week moving average and rm NAs from time-series

  rolling_avg = zoo::rollmean(inc, k = 7, fill = NA, align = 'center')
  na_ind <- which(is.na(rolling_avg))
  rolling_avg = rolling_avg[-na_ind]

  I0_min = 10

  if (length(ind) == 1) { # covid

    ## we choose to draw:

    config <- make_config(list(mean_si = 6.48, std_mean_si = 3.83, min_mean_si = 2.48,
                               max_mean_si = 10.48, std_si = 10, std_std_si = 1,
                               min_std_si = 1, max_std_si = 19))
    method <- "uncertain_si"

  } else { # influenza

    config <- make_config(list(mean_si = 2.6,std_si = 1.5))
    method <- "parametric_si"
  }



  res_parametric_si <- suppressMessages(estimate_R(rolling_avg,
                                                   method=method, config = config))


  I0 <- res_parametric_si$I_imported[1]
  I0 <- round(max(I0, I0_min))

  return(I0)

}

#' Set initial values for model states - SIRH or SEIRH
#'
#' Based on the population size and the initial number of cases initialize the
#' number in each compartment
#' @param pop population size, integer
#' @param I0 estimate for initial number of cases
#' @param mymodel model name sirh or seirh
#' @return init_state named list with initial number of individuals in each compartment:
#'
#' S0 - susceptible
#'
#' E0 - exposed (seirh only)
#'
#' I0 - infectious
#'
#' R0 - recovered
#'
#' H0 - hospitalized
#'
#' @keywords internal
set_init_states <- function(pop, I0, mymodel) {

  # for safety
  pop = round(pop)

  I0 = round(I0)

  if (mymodel == 'sirh') {
    S0 = pop - I0
    R0 = 0
    H0 = 0
    state0 = list(S0 = S0, I0 = I0, R0 = R0, H0 = H0)
  } else {
    E0 = I0
    S0 = pop - (I0 + E0)
    R0 = 0
    H0 = 0
    state0 = list(S0 = S0, E0 = E0, I0 = I0, R0 = R0, H0 = H0)
  }

  return(state0)

}

#' Set disease specific initial guess for some model parameters
#'
#' Based on the disease set the initial guess for model parameters
#' @param mymodel model name sirh or seirh
#' @param mydisease disease name covid19 or influenza
#' @param inc time-series of hospitalization
#' @return param a model specific list with initial guess for parameters:
#'
#' gamma - generation time
#'
#' mu_H1H2 - holding time between sub-compartments 1 and 2 of H
#'
#' Beta - force of infection
#'
#' pH - hospitalization probability
#'
#' rho - reporting rate (0.95)
#'
#' mu_EI - latency period (seirh only)
#'
#' @keywords internal

init_param <- function(mymodel, mydisease, inc) {

  if (mymodel == 'sirh' & mydisease == 'influenza') { #influenza  and SIRH
    gamma = 2.6
    mu_H1H2 = 1.0
    Beta = 1.3 / gamma
    pH = 0.001
    rho = 0.95
    wl = 3.0
    param = list(gamma = 1./ gamma, mu_H1H2 = 1./mu_H1H2, Beta = Beta, pH = pH, rho = rho,
                 wl = wl)
  } else if (mymodel == 'seirh' & mydisease == 'covid19') { # covid19 and SEIRH
    gamma = 4.0
    mu_H1H2 = 1.0
    if (max(inc) <= 15) {
      Beta = 1.05 / gamma
    } else if ( max(inc) < 35 & max(inc) > 15) {
      Beta = 1.2/ gamma
    } else {
      Beta = 0.5
    }
    mu_EI = 1.0
    pH = 0.01
    rho = 0.95
    wl = 3.0
    param = list(gamma = 1./gamma, mu_H1H2 = 1./mu_H1H2, mu_EI = 1./mu_EI, Beta = Beta,
                 pH = pH, rho = rho, wl = wl)
  } else if (mymodel == 'seirh' & mydisease == 'influenza') { #influenza  and SEIRH
    gamma = 2.6
    mu_H1H2 = 1.0
    Beta = 1.2/gamma
    mu_EI = 1.0
    pH = 0.001
    rho = 0.95
    wl = 3.0
    param = list(gamma = 1./gamma, mu_H1H2 = 1./mu_H1H2, mu_EI = 1./mu_EI, Beta = Beta,
                 pH = pH, rho = rho, wl = wl)
  } else if (mymodel == 'sirh' & mydisease == 'covid19') { # covid19 and SIRH
    if (max(inc) <= 15) {
      Beta = 1.05 / gamma
    } else if ( max(inc) < 35 & max(inc) > 15) {
      Beta = 1.2/ gamma
    } else {
      Beta = 0.5
    }
    mu_EI = 1.0
    pH = 0.01
    rho = 0.95
    wl = 3.0
    param = list(gamma = 1./ gamma, mu_H1H2 = 1./mu_H1H2, Beta = Beta, pH = pH, rho = rho,
                 wl = wl)
  } else {
    stop('Can not Find Model or Disease \n')
  }

  return(param)
}

#' Set value of baseline incidence
#'
#' Early in the season use first two weeks of data and later use last week
#' of data.  Early/late is determined by value of end_of_season
#'
#' @param inc incidence time series
#' @param end_of_season logical (TRUE or FALSE)
#' @return baseline integer baseline value >= 1
#' @keywords internal
#'
get_baseline <-function(inc, end_of_season) {

  nstart = 14

  baseline = round(mean(inc[1:14]))

  if (end_of_season) {
    npast = 7
    ntimes = length(inc)
    baseline = round(mean(inc[(ntimes-npast):ntimes]))
  }
  baseline = max(baseline, 1)
  return(baseline)
}

#' Run the optim package to improve initial guess for parameters
#'
#' For SIRH model use the optim package to obtain an improved initial
#' guess for some model parameters.  This is a simple optimization with a
#' fixed one value for the force of infection
#'
#' @param param0  initial guess for model parameters
#' @param inc hospitalization incidence
#' @return improved initial guess for some of the model parameters
#' @keywords internal

run_optim <- function(param0, inc) {
  ntimes = length(inc)
  time = 1:ntimes
  data = data.frame(time = time, cases = inc)
  # start definition of pomp object
#
#
#   coef(flu) <- c(Beta = param0$Beta, gamma = 1./param0$gamma,
#                  mu_H1H2 = param0$mu_H1H2, pH = param0$pH, pop = param0$pop, I0 = param0$I0,
#                  S0 = param0$S0, R0 = param0$R0,
#                  rho = param0$rho, baseline = param0$baseline)

  # This object is defined with the deterministic ODE's - fixed beta

  pomp(data=data,
       times="time",t0=0,
       skeleton = vectorfield(sirh.ode),
       accumvars = c("Ic", "Ih"),
       rinit=init,
       obsnames="cases",
       statenames=c("S","I","R","H1","H2","Ic","Ih"),
       paramnames=names(param0)) -> flu2

  f1 <- function (par) {
    params <- c(Beta = exp(par[1]), gamma =  param0$gamma,
                mu_H1H2 = exp(par[2]), pH = expit(par[3]), pop = param0$pop, I0 = param0$I0,
                S0 = param0$S0, R0 = param0$R0,
                rho = param0$rho, baseline = param0$baseline)
    -poisson.loglik(params)
  }

  poisson.loglik <- function (params) {
    x <- trajectory(flu2,params=params, format= 'data.frame')
    lambda = unlist(param0$rho*x[,'Ih']+ param0$baseline)
    sum(dpois(x=data[,'cases'],lambda=lambda,log=TRUE), na.rm = TRUE)
  }

  # start a simple optimization
  fit1 <- optim(f1,par=c(log(param0$Beta),log(param0$mu_H1H2),logit(param0$pH)),
                control=list(maxit=500)) #, method = 'SANN')#, lower = c(1.,0.,-7),
                #upper = c(1.3, 1.1, -2)) #, method = "SANN")

  mle1 <- c(Beta=exp(fit1$par[1]),mu_H1H2=exp(fit1$par[2]), pH = expit(fit1$par[3]))

  # print(mle1)
  params1 <- c(mle1, param0[c('gamma', 'pop','S0','I0','R0','rho','baseline')])

  # For debug purposes generate a determinstic trajectory with the parameters
  model.pred <- trajectory(flu2, params=params1, format = 'data.frame')
  model.pred[,'cases'] = rpois(ntimes, model.pred[,'Ih'] * params1$rho + params1$baseline)
  model.pred[,'obs'] = data[,'cases']

  return(params1)

}

#'
#' Set initial values for time-dependent Force of Infection
#'
#' Set the values for the force of infection and the time(s) change takes place
#'
#' @param nb integer number of unique values FOI can have
#' @param ntimes integer length of time series
#' @param Beta real single initial guess value for the FOI
#' @return td_beta a list with two items:
#' betac - array with initial guesses for beta
#' tcng. - array with initial guesses for time of change in beta
#' @keywords internal
#'
set_td_beta <- function(nb, ntimes, Beta) {

  beta_vec = rep(Beta, nb)
  beta_vec = runif(nb,Beta*0.95, Beta*1.05)
  end_period = ntimes/nb
  tcng_vec = rep(round(end_period/2), nb)

  tcng_vec[nb] = 0.0

  list1 = list2 = list()
  for (i in 1:nb) {
    name1 = paste0('Beta', i)
    list1[[name1]] = beta_vec[i]

    name2 = paste0('tcng', i)
    list2[[name2]] = tcng_vec[i]
  }

  td_beta = list(beta = list1, tcng = list2)
  return(td_beta)
}

#'
#' Set model parameter names
#'
#' Based on the model and number of values FOI can have set parameter names
#'
#' @param nb integer number of unique values FOI can have
#' @param par_names vector with model specific parameter names
#' @return param_info_list a list with three items:
#' nparam integer number of parameters excluding td-foi
#' nparamtot integer total number of parameters including td-foi
#' parnames complete list of all parameter names
#' @keywords internal
#'
update_par_names <- function(nb, par_names) {


  nparam = length(par_names[!par_names %in% "Beta"])

  nparamtot = nparam + nb * 2

  betanames = paste0('Beta', 1:nb)
  tcngnames = paste0('tcng', 1:nb)

  par = rep(NA, nparamtot)

  # full list of parameter names including the time-dependent beta

  par_names = c(par_names[!par_names %in% "Beta"], betanames, tcngnames)

  param_info_list = list(nparam = nparam, nparamtot = nparamtot,
                         par_names = par_names)

  return(param_info_list)

}

#'
#' Create a days number array from a dates array
#'
#' @param dates array of dates for incidence
#' @return times - an array of day numbers
#'
dates_to_int <- function(dates) {

  int_dates = as.numeric(dates)

  times = int_dates - int_dates[1] + 1

  return(times)
}


#'
#' Basic plotting function
#'
#' @param prof_data - the complete data structure
#' @param filename - if NULL print to screen else save to PDF
#' @return plot object to screen or to file
#'
plot_prof_data <- function(prof_data, filename = NULL) {

  npath = length(prof_data)

  col = c('salmon', 'cornflowerblue')
  pl = list()


  for (ip in 1:npath) {

    mydata = prof_data[[ip]]

    reg_name = mydata$loc_name
    disease = mydata$disease
    inc = mydata$data$inc
    dates  = mydata$data$date
    inc_type = mydata$inc_type

    alpha_data = alpha_fit = 1.0
    if (!is.null(mydata$data_fit)) {
      dates_fit = mydata$data_fit$date
      inc_fit   = mydata$data_fit$inc
      data_fit = data.frame(x=dates_fit, y = inc_fit)
      alpha_data = 0.2
    }


    cadence = as.numeric(dates[2]-dates[1])
    if (cadence == 1) cadence_lab = 'Daily'
    if (cadence == 7) cadence_lab = 'Weekly'

    main_txt = paste0(reg_name, ' - ', disease,' ',cadence_lab)

    start_year = lubridate::year(range(dates)[1])
    end_year   = start_year + 1
    xlab = paste0(start_year,' - ', end_year)
    ylab = inc_type
    data = data.frame(x=dates, y = inc)

    pl[[ip]] <- ggplot() +
      geom_point(data = data, aes(x,y), color=col[ip], alpha = alpha_data, size = 3) +
      labs(x = xlab,           # X-axis label
           y = ylab,           # Y-axis label
           title = main_txt)  # Main title

    if (!is.null(mydata$data_fit)) {
      pl[[ip]] <- pl[[ip]] +
        geom_point(data = data_fit, aes(x,y), color = col[ip], alpha = alpha_fit, size = 3) +
        geom_vline(xintercept=max(data_fit$x), linetype = 'dashed', col = 'darkgreen')

    }

  }

  interactive_plot <- list()

  for (ip in 1:npath) {
    interactive_plot[[ip]] <- ggplotly(pl[[ip]])
  }

  if (npath == 2) {
    # suppressWarnings(print(grid.arrange(pl[[1]], pl[[2]], ncol = 2)))
    arrange_plot <- subplot(interactive_plot[[1]], interactive_plot[[2]], nrows = 1, titleX = TRUE, titleY = TRUE)

    if (!is.null(filename)) {
      suppressWarnings(print(grid.arrange(pl[[1]], pl[[2]], ncol = 2)))
      ggsave(filename = filename, plot = grid_plots, width = 14, height = 6, dpi = 300)
      cat("\n Saving Fit Plots to: ", filename,'\n')
    }
  } else {
    # suppressWarnings(pl[[1]])
    arrange_plot <- interactive_plot[[1]]

    if (!is.null(filename)) {
      ggsave(filename = filename, plot = last_plot(), width = 7, height = 6, dpi = 300)
      cat("\n Saving Fit Plots to: ", filename,'\n')
    }

  }
 return(arrange_plot)
}

#'
#' Combine COVID19 and Influenza Forecasts
#'
#' Forecasts are combined using two procedures: random addition of trajectories and perfect
#' ordering of trajectories
#'
#' @param prof_data - the complete data structure
#' @param dates_frcst_list - a list of length two with dates (fit and forecast) for each
#' pathogen
#' @param simdat_list - a list of length two with an array of trajectories for each pathogen
#'
#' @return a list with two items
#' simdat_both - a list of length two with the two estimates for the combined burden
#' (random and ordered)
#' inc_both - the combined reported hospitalization
#' dates_both - dates array for combined forecast
#'
#'
combine_forecasts <- function(prof_data = NULL, dates_frcst_list = NULL, simdat_list = NULL) {

  if (is.null(prof_data)) stop('Missing prof_data in combined_foreccasts')
  if (is.null(dates_frcst_list)) stop('Missing dates_frcst_list in combined_foreccasts')
  if (is.null(simdat_list)) stop('Missing simdat_list in in combined_foreccasts')


  npath = length(names(prof_data))

  # find dates that are common to both diseases
  # find number of trajectories in each forecast

  dates_start = dates_end = rep(as.Date("2020-01-01"), npath)
  ntraj_both = rep(0, npath)

  for (ip in 1:npath) {
    dates_start[ip] = min(dates_frcst_list[[ip]])
    dates_end[ip]   = max(dates_frcst_list[[ip]])
    ntraj_both[ip] = dim(simdat_list[[ip]])[1]
  }

  start_date = max(dates_start)
  end_date   = min(dates_end)

  # Dates array for both pathogens
  dates_both = seq(start_date, end_date, by = '1 day')

  ntraj_both = min(ntraj_both)

  # subset each pathogen using start/end dates

  for (ip in 1:npath) {

    mydata = prof_data[[ip]]

    # hospitalization incidence - fitted
    inc = mydata$data_fit$inc

    # dates
    dates  = mydata$data_fit$date

    # complete hospitalization incidence and dates

    inc_all = mydata$data$inc
    dates_all  = mydata$data$date

    dates_frcst = dates_frcst_list[[ip]]

    ind0 = which(dates_frcst == start_date)
    ind1 = which(dates_frcst ==   end_date)

    if (length(inc_all) >= length(dates_frcst)) {
      inc = inc_all[ind0:ind1]
    }  else {
      inc = inc_all[ind0:length(inc_all)]
    }

    inc_fit_all =  mydata$data_fit$inc

    if (length(inc_fit_all) >= length(dates_frcst)) {
      inc_fit = inc_fit_all[ind0:ind1]
    }  else {
      inc_fit = inc_fit_all[ind0:length(inc_fit_all)]
    }

    simdat = simdat_list[[ip]]

    simdat = simdat[1:ntraj_both, ind0:ind1]

    simdat_list[[ip]] = simdat

    if (ip == 1) {
      rand_simdat_both = ordered_simdat_both = simdat * 0.0
      obs_both = inc * 0.0
      obs_fit_both = inc_fit * 0.0
    }

    rand_simdat_both = rand_simdat_both + simdat
    obs_both = obs_both + inc

    obs_fit_both = obs_fit_both + inc_fit

    # now order also

    max_values <- apply(simdat, 1, max)
    # print(length(max_values))
    ordered_simdat <- simdat[order(max_values),]
    ordered_simdat_both = ordered_simdat_both + ordered_simdat

  }

  simdat_both = list()
  simdat_both[['random']]  = rand_simdat_both
  simdat_both[['ordered']] = ordered_simdat_both

  return(list(simdat_both = simdat_both, obs_both = obs_both, obs_fit_both = obs_fit_both, dates_both = dates_both))
}

#'
#' Fit a baseline statistical model to the data
#'
#' See: file:///Users/michal/Downloads/pnas.2113561119.sapp.pdf for more details (start at bottom of page 3)
#' This baseline model forecasts a predictive median incidence equal to the incidence in the
#' most recent data point(s), with uncertainty around the median based on changes in incidence
#' that were observed in the past of the time series.
#'
#' Note that we do not force the median of the forecast forecast to be equal to
#' the last observed value since the data is likely to be daily.  But we do truncate the distribution
#' so that it has no negative values.
#' Adjustments are made for daily data (e.g., we use the last seven data points for the forecast)
#'
#' @param inc integer. Observed pathogen incidence
#' @param ntraj integer. Requested number of trajectories
#' @return A 2D array of dimension (ntraj, ntimes) with the trajectories (ntimes is the incidence length)
#

stat_fit <- function(inc, ntraj) {

  ntimes = length(inc)

  simdat = array(0, c(ntraj, ntimes))

  data_last = inc[ntimes]

  noise_p = diff(inc)
  noise_n = - noise_p
  noise = c(noise_p, noise_n)

  ecdf_noise <- ecdf(noise)

  piecewise_linear <- function(n) {
    u <- runif(n)
    x <- quantile(noise, u)
    y <- ecdf_noise(x)
    data.frame(x = x, y = y)
  }

  for (ii in 1:ntraj) {
    my_noise = piecewise_linear((ntimes))
    my_cases = inc + my_noise$x
    my_cases = pmax(my_cases,0) # ensure it is not negative
    simdat[ii,] = my_cases
  }

  return(simdat)
}

#'
#' Forecast using a baseline statistical model fitted to the data
#'
#' See: file:///Users/michal/Downloads/pnas.2113561119.sapp.pdf for more details (start at bottom of page 3)
#' This baseline model forecasts a predictive median incidence equal to the incidence in the
#' most recent data point(s), with uncertainty around the median based on changes in incidence
#' that were observed in the past of the time series.
#' Adjustments are made for daily data (e.g., we use the last seven data points for the forecast)
#' Note that we do not force the median of the forecast forecast to be equal to
#' the last observed value since the data is likely to be daily.  But we do truncate the distribution
#' so that it has no negative values.
#'
#' @param data entire data structure of the pathogen
#' @param ntraj integer. Number of requested trajectories
#' @param nfrcst integer. Number of requested forecast horizons
#'
#' @return simdat a 2D array of dimension (ntraj, (ntimes+nfrcst))

stat_forecast <- function(data, ntraj, nfrcst) {

  dates = data$data_fit$date
  inc = data$data_fit$inc

  # using the date array build an integer day array

  times = dates_to_int(dates)

  ntimes = length(times)

  ntimes_frcst= ntimes + nfrcst

  # build also the arrays for the forecasts

  cadence = times[2] - times[1]

  if (cadence == 1) {
    cadence_lab = paste0(cadence, ' day')
    print_lab = 'Days'
    dates_frcst = seq(from = dates[1], length = ntimes_frcst, by = '1 day')
  }

  if (cadence == 7) {
    cadence_lab = paste0(cadence, ' week')
    print_lab = 'Weeks'
    dates_frcst = seq(from = dates[1], length = ntimes_frcst, by = '1 week')
  }

  if (cadence == 7) {
    data_last = inc[ntimes]
  } else {
    data_last = inc[(ntimes-7+1):ntimes]
  }

  noise_p = diff(inc)
  noise_n = - noise_p
  noise = c(noise_p, noise_n)

  ecdf_noise <- ecdf(noise)

  piecewise_linear <- function(n) {
    u <- runif(n)
    x <- quantile(noise, u)
    y <- ecdf_noise(x)
    data.frame(x = x, y = y)
  }

  simdat <- array(0, c(ntraj, ntimes_frcst))

  # figure out how many times data_last needs to be added to inc
  nadd = floor(nfrcst/length(data_last))
  inc_frcst = c(inc, rep(data_last, nadd))

  if (length(inc_frcst) < ntimes_frcst) {
    nadd2 = ntimes_frcst - length(inc_frcst)
    ind = sample(data_last, nadd2)
    inc_frcst = c(inc_frcst, data_last[ind])
  }

  for (ii in 1:ntraj) {
    my_noise = piecewise_linear((ntimes_frcst))
    my_cases = inc_frcst + my_noise$x
    my_cases = pmax(my_cases,0) # ensure it is not negative
    simdat[ii,] = my_cases
  }

  return(simdat)
}


# C functions

# define deterministic and stochastic Euler time steps

#'
#' Deterministic SIR Euler Time Step
#'
#' Advance SIR model by one deterministic time step
#'
#'@return change in each compartment, real
#'
sir.ode <- Csnippet("
  DS = -Beta*S*I/N;
  DI = Beta*S*I/N-gamma*I;
  DR = (1-pH)*gamma*I;
  DH1 = pH*gamma*I - mu_H1H2*H1;
  DH2 = mu_H1H2 * H1;
  DIc = Beta*S*I/N;
  DIh = mu_H1H2*H1;
")

#'
#' Stochastic SIR Euler Time Step
#'
#' Advance SIR model by one stochastic time step
#'
#' @return change in each compartment, integer
#'
sir.step <- "
  double P;
  P = S + I + H1 + H2 + R;
  double t1 = rbinom(S, 1-exp(-Beta*I/P*dt));
  double t2 = rbinom(I, 1-exp(-gamma*dt));
  double t3 = rbinom(H1, 1-exp(-mu_H1H2*dt));

  S +=-t1;
  I += t1 - t2;
  R  += round((1.-pH) * t2);
  H1 += round(pH * t2) - t3;
  H2 += t3;

  Ic += t1;
  Ih += t3;
"

#'
#' Deterministic SIRH Euler Time Step
#'
#' Advance SIRH model by one deterministic time step
#'
#'@return change in each compartment, real
#'
#'
sirh.ode <- Csnippet("
  DS = -Beta*S*I/pop;
  DI = Beta*S*I/pop-gamma*I;
  DR = (1-pH)*gamma*I;
  DH1 = pH*gamma*I - mu_H1H2*H1;
  DH2 = mu_H1H2 * H1;
  DIc = Beta*S*I/pop;
  DIh = mu_H1H2*H1;
")

#'
#' Stochastic SIRH Euler Time Step
#'
#' Advance SIRH model by one stochastic time step
#'
#'@return change in each compartment, real
#'
#'
sirh.step <- "
  double P;
  P = S + I + H1 + H2 + R;
  double t1 = rbinom(S, 1-exp(-Beta*I/P*dt));
  double t2 = rbinom(I, 1-exp(-gamma*dt));
  double t3 = rbinom(H1, 1-exp(-mu_H1H2*dt));

  S +=-t1;
  I += t1 - t2;
  R  += round((1.-pH) * t2);
  H1 += round(pH * t2) - t3;
  H2 += t3;

  Ic += t1;
  Ih += t3;
"

#'
#' Stochastic TD-SIRH Euler Time Step
#'
#' Advance TD-SIRH model by one stochastic time step
#' This version is for a FOI with two values
#'
#'@return change in each compartment, real
#'
#'
td.sirh.step <- "
  double P;
  double beta_cur;
  P = S + I + H1 + H2 + R;

  beta_cur = Beta1 + Beta2;
  beta_cur = beta_cur + (Beta2 - Beta1)* tanh((time-tcng1)/wl);
  beta_cur = beta_cur * 0.5;

  double t1 = rbinom(S, 1-exp(-beta_cur*I/P*dt));
  double t2 = rbinom(I, 1-exp(-gamma*dt));
  double t3 = rbinom(H1, 1-exp(-mu_H1H2*dt));

  S +=-t1;
  I += t1 - t2;
  R  += round((1.-pH) * t2);
  H1 += round(pH * t2) - t3;
  H2 += t3;
  Ic += t1;
  Ih += t3;
  time += dt;
"

#'
#' Stochastic TD-SIRH Euler Time Step
#'
#' Advance TD-SIRH model by one stochastic time step
#' This version is for a FOI with three values
#'
#'@return change in each compartment, real
#'
#'
td3.sirh.step <- "
  double P;
  double beta_cur;
  P = S + I + H1 + H2 + R;

  beta_cur = Beta1 + Beta3;
  beta_cur = beta_cur + (Beta2 - Beta1)* tanh((time-tcng1)/wl);
  beta_cur = beta_cur + (Beta3 - Beta2)* tanh((time-tcng2)/wl);
  beta_cur = beta_cur * 0.5;

  double t1 = rbinom(S, 1-exp(-beta_cur*I/P*dt));
  double t2 = rbinom(I, 1-exp(-gamma*dt));
  double t3 = rbinom(H1, 1-exp(-mu_H1H2*dt));

  S +=-t1;
  I += t1 - t2;
  R  += round((1.-pH) * t2);
  H1 += round(pH * t2) - t3;
  H2 += t3;
  Ic += t1;
  Ih += t3;
  time += dt;
"

#'
#' Deterministic TD-SIRH Euler Time Step
#'
#' Advance TD-SIRH model by one deterministic time step
#'
#'@return change in each compartment, real
#'
#'
td.sirh.ode <- Csnippet("
  double beta_cur;

  beta_cur = Beta1 + Beta2;
  beta_cur = beta_cur + (Beta2 - Beta1)* tanh((time-tcng1)/wl);
  beta_cur = beta_cur * 0.5;

  DS = -beta_cur*S*I/pop;
  DI = beta_cur*S*I/pop-gamma*I;
  DR = (1-pH)*gamma*I;
  DH1 = pH*gamma*I - mu_H1H2*H1;
  DH2 = mu_H1H2 * H1;
  DIc = beta_cur*S*I/pop;
  DIh = mu_H1H2*H1;
  Dtime  = 1;
")

#'
#' Deterministic SEIRH Euler Time Step
#'
#' Advance SEIRH model by one deterministic time step
#'
#'@return change in each compartment, real
#'
#'
seirh.ode <- Csnippet("
  DS = -Beta*S*I/N;
  DE = Beta*S*I/N - mu_EI*E;
  DI = mu_EI*E-gamma*I;
  DR = (1-pH)*gamma*I;
  DH1 = pH*gamma*I - mu_H1H2*H1;
  DH2 = mu_H1H2 * H1;
  DIc = mu_EI*E;
  DIh = mu_H1H2*H1;")

#'
#' Deterministic TD-SEIRH Euler Time Step
#'
#' Advance TD-SEIRH model by one deterministic time step
#' This version is for a two value FOI
#'
#'@return change in each compartment, real
#'
#'
td.seirh.ode <- Csnippet("
  double beta_cur;

  beta_cur = Beta1 + Beta2;
  beta_cur = beta_cur + (Beta2-Beta1) * tanh((time-tcng1)/wl);
  beta_cur = beta_cur * 0.5;

  DS = -beta_cur*S*I/pop;
  DE = beta_cur*S*I/pop - mu_EI*E;
  DI = mu_EI*E-gamma*I;
  DR = (1-pH)*gamma*I;
  DH1 = pH*gamma*I - mu_H1H2*H1;
  DH2 = mu_H1H2 * H1;
  DIc = mu_EI*E;
  DIh = mu_H1H2*H1;
  Dtime = 1;")

#'
#' Deterministic TD-SEIRH Euler Time Step
#'
#' Advance TD-SEIRH model by one deterministic time step
#' This version is for a three value FOI
#'
#'@return change in each compartment, real
#'
#'
td3.seirh.ode <- Csnippet("
  double beta_cur;

  beta_cur = Beta1 + Beta3;
  beta_cur = beta_cur + (Beta2-Beta1) * tanh((time-tcng1)/wl);
  beta_cur = beta_cur + (Beta3-Beta2) * tanh((time-(tcng1+tcng2))/wl);
  beta_cur = beta_cur * 0.5;

  DS = -beta_cur*S*I/pop;
  DE = beta_cur*S*I/pop - mu_EI*E;
  DI = mu_EI*E-gamma*I;
  DR = (1-pH)*gamma*I;
  DH1 = pH*gamma*I - mu_H1H2*H1;
  DH2 = mu_H1H2 * H1;
  DIc = mu_EI*E;
  DIh = mu_H1H2*H1;
  Dtime = 1;")

#' Stochastic SEIRH Euler Time Step
#'
#' Advance SEIRH model by one stochastic time step
#'
#'@return change in each compartment, integer
#'
#'
seirh.step <- "
  double P;
  P = pop;
  double t1 = rbinom(S, 1-exp(-Beta*I/P*dt));
  double t2 = rbinom(E, 1-exp(-mu_EI*dt));
  double t3 = rbinom(I, 1-exp(-gamma*dt));
  double t4 = rbinom(H1, 1-exp(-mu_H1H2*dt));

  S +=-t1;
  E +=t1 - t2;
  I +=t2 - t3;
  R  += round((1.-pH) * t3);
  H1 += round(pH * t3) - t4;
  H2 += t4;

  Ic += t2;
  Ih += t4;
"

#' Stochastic TD-SEIRH Euler Time Step
#'
#' Advance TD-SEIRH model by one stochastic time step
#' This code is for a two value FOI
#'
#'@return change in each compartment, integer
#'
#'
td.seirh.step <- "
  double P;
  double beta_cur;
  P = pop;

  beta_cur = Beta1 + Beta2;
  beta_cur = beta_cur + (Beta2-Beta1) * tanh((time-tcng1)/wl);
  beta_cur = beta_cur * 0.5;

  double t1 = rbinom(S, 1-exp(-beta_cur*I/P*dt));
  double t2 = rbinom(E, 1-exp(-mu_EI*dt));
  double t3 = rbinom(I, 1-exp(-gamma*dt));
  double t4 = rbinom(H1, 1-exp(-mu_H1H2*dt));

  S +=-t1;
  E +=t1 - t2;
  I +=t2 - t3;
  R  += round((1.-pH) * t3);
  H1 += round(pH * t3) - t4;
  H2 += t4;

  Ic += t2;
  Ih += t4;
  time += dt;
"

#' Stochastic TD-SEIRH Euler Time Step
#'
#' Advance TD-SEIRH model by one stochastic time step
#' This code is for a 3 value FOI
#'
#'@return change in each compartment, integer
#'
#'
td3.seirh.step <- "
  double P;
  double beta_cur;
  P = pop;

  beta_cur = Beta1 + Beta3;
  beta_cur = beta_cur + (Beta2-Beta1) * tanh((time-tcng1)/wl);
  beta_cur = beta_cur + (Beta3-Beta2) * tanh((time-(tcng1+tcng2))/wl);
  beta_cur = beta_cur * 0.5;

  double t1 = rbinom(S, 1-exp(-beta_cur*I/P*dt));
  double t2 = rbinom(E, 1-exp(-mu_EI*dt));
  double t3 = rbinom(I, 1-exp(-gamma*dt));
  double t4 = rbinom(H1, 1-exp(-mu_H1H2*dt));

  S +=-t1;
  E +=t1 - t2;
  I +=t2 - t3;
  R  += round((1.-pH) * t3);
  H1 += round(pH * t3) - t4;
  H2 += t4;

  Ic += t2;
  Ih += t4;
  time += dt;
"

# define initial conditions

#' Initial Conditions SIRH Model
#'
#'
#'@return Initial values of all compartments
#'

init <- Csnippet("
  S = round(S0);
  I = round(I0);
  R = round(R0);
  H1 = 0;
  H2 = 0;
  Ic = 0;
  Ih = 0;
  ")

#' Initial Conditions TD-SIRH Model
#'
#'
#'@return Initial values of all compartments
#'

td.init <- Csnippet("
  S = round(S0);
  I = round(I0);
  R = round(R0);
  H1 = 0;
  H2 = 0;
  Ic = 0;
  Ih = 0;
  time = 0;
  ")

#' Initial Conditions SEIRH Model
#'
#'
#'@return Initial values of all compartments
#'
#'
init.seirh <- Csnippet("
  S = round(S0);
  E = round(E0);
  I = round(I0);
  R = round(R0);
  H1 = 0;
  H2 = 0;
  Ic = 0;
  Ih = 0;
  ")

#' Initial Conditions TD-SEIRH Model
#'
#'
#'@return Initial values of all compartments
#'
#'
td.init.seirh <- Csnippet("
  S = round(S0);
  E = round(E0);
  I = round(I0);
  R = round(R0);
  H1 = 0;
  H2 = 0;
  Ic = 0;
  Ih = 0;
  time = 0;
  ")

# rho is the reporting fraction

#'
#' Log-Likelihood calculation
#'
#' @return LLK
dmeas <- Csnippet("
  lik = dpois(cases,rho*Ih+1e-6+baseline,give_log);
")


#'
#' Given the number in the H compartment the reporting
#' probability and and baseline value calculate the
#' number of cases using Poisson sampling
#' @return cases, integer
#'
rmeas <- Csnippet("
  cases = rpois(rho*Ih+1e-6+baseline);
")

#'
#' Transform variables before optimization
#'
#' @return transformed variables
#'
toEst <- Csnippet("
 TI0  = log(I0);
 TBeta = log(Beta);
 Tmu_IH = log(mu_IH);
 Trho = logit(rho);
 TpH  = logit(pH);
 Tbaseline = log(baseline);
")

#'
#' Transform variables back
#'
#' @return variables transformed back
#'
fromEst <- Csnippet("
 TI0  = exp(I0)
 TBeta = exp(Beta);
 Tmu_IH = exp(mu_IH);
 Trho = expit(rho);
 TpH  = expit(pH);
 Tbaseline = exp(baseline);
")

#' Deterministic ODEs for SEIRH model with a
#' 3-value FOI
#' @param t - array of times
#' @param y - array of states
#' @param parms - array of paramters inclucing 'wl'
#'
#' @return res - an array with the derivatives of y
#'
td3_seirh_dynamics <- function(t, y, parms) {

dy = array(0, length(y))

beta_cur = parms['Beta1'] + parms['Beta3']
beta_cur = beta_cur + (parms['Beta2'] - parms['Beta1']) * tanh((t-parms['tcng1'])/parms['wl'])
beta_cur = beta_cur + (parms['Beta3'] - parms['Beta2']) * tanh((t-(parms['tcng1']+parms['tcng2']))/parms['wl'])
beta_cur = beta_cur * 0.5

rate = beta_cur * y[1] * y[3] /parms['pop']
# S, E, I, R H1, H2
dy[1] = -rate
dy[2] = rate - parms['mu_EI'] * y[2]
dy[3] = parms['mu_EI'] * y[2] - parms['gamma'] * y[3]
dy[4] = (1.0 - parms['pH']) * parms['gamma'] * y[3]
dy[5] = parms['pH'] * parms['gamma'] * y[3] - parms['mu_H1H2'] * y[5]
dy[6] = parms['mu_H1H2'] * y[5]

res = list(dy)
return(res)

}

#' Deterministic ODEs for SEIRH model with a
#' 2-value FOI
#' @param t - array of times
#' @param y - array of states
#' @param parms - array of paramters inclucing 'wl'
#'
#' @return res - an array with the derivatives of y
#'
td2_seirh_dynamics <- function(t, y, parms) {

  dy = array(0, length(y))

  beta_cur = parms['Beta1'] + parms['Beta2']
  beta_cur = beta_cur + (parms['Beta2'] - parms['Beta1']) * tanh((t-parms['tcng1'])/parms['wl'])
  beta_cur = beta_cur * 0.5

  rate = beta_cur * y[1] * y[3] /parms['pop']
  dy[1] = -rate
  dy[2] = rate - parms['mu_EI'] * y[2]
  dy[3] = parms['mu_EI'] * y[2] - parms['gamma'] * y[3]
  dy[4] = (1.0 - parms['pH']) * parms['gamma'] * y[3]
  dy[5] = parms['pH'] * parms['gamma'] * y[3] - parms['mu_H1H2'] * y[5]
  dy[6] = parms['mu_H1H2'] * y[5]

  res = list(dy)
  return(res)

}

#' Deterministic ODEs for SIRH model with a
#' 3-value FOI
#' @param t - array of times
#' @param y - array of states
#' @param parms - array of paramters inclucing 'wl'
#'
#' @return res - an array with the derivatives of y
#'
td3_sirh_dynamics <- function(t, y, parms) {

  dy = array(0, length(y))

  beta_cur = parms['Beta1'] + parms['Beta3']
  beta_cur = beta_cur + (parms['Beta2'] - parms['Beta1']) * tanh((t-parms['tcng1'])/parms['wl'])
  beta_cur = beta_cur + (parms['Beta3'] - parms['Beta2']) * tanh((t-(parms['tcng1']+parms['tcng2']))/parms['wl'])
  beta_cur = beta_cur * 0.5

  rate = beta_cur * y[1] * y[2] /parms['pop']
  # S, I, R H1, H2
  rate = beta_cur * y[1] * y[2] /parms['pop']
  dy[1] = -rate
  dy[2] = rate - parms['gamma'] * y[2]
  dy[3] = (1.0 - parms['pH']) * parms['gamma'] * y[2]
  dy[4] = parms['pH'] * parms['gamma'] * y[2] - parms['mu_H1H2'] * y[4]
  dy[5] = parms['mu_H1H2'] * y[4]

  res = list(dy)
  return(res)

}

#' Deterministic ODEs for SIRH model with a
#' 2-value FOI
#' @param t - array of times
#' @param y - array of states
#' @param parms - array of paramters inclucing 'wl'
#'
#' @return res - an array with the derivatives of y
#'
td2_sirh_dynamics <- function(t, y, parms) {

  dy = array(0, length(y))

  beta_cur = parms['Beta1'] + parms['Beta2']
  beta_cur = beta_cur + (parms['Beta2'] - parms['Beta1']) * tanh((t-parms['tcng1'])/parms['wl'])
  beta_cur = beta_cur * 0.5

  rate = beta_cur * y[1] * y[2] /parms['pop']
  dy[1] = -rate
  dy[2] = rate - parms['gamma'] * y[2]
  dy[3] = (1.0 - parms['pH']) * parms['gamma'] * y[2]
  dy[4] = parms['pH'] * parms['gamma'] * y[2] - parms['mu_H1H2'] * y[4]
  dy[5] = parms['mu_H1H2'] * y[4]

  res = list(dy)
  return(res)

}



