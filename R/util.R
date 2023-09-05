
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
init_param <- function(mymodel, inc) {

  if (mymodel == 'sirh') { #influenza
    gamma = 2.6
    mu_H1H2 = 1.0
    Beta = 1.3 / gamma
    pH = 0.001
    rho = 0.95
    wl = 3.0
    param = list(gamma = 1./ gamma, mu_H1H2 = 1./mu_H1H2, Beta = Beta, pH = pH, rho = rho,
                 wl = wl)
  } else { # covid19
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
  tcng_vec = rep(round(ntimes/(nb*2)), nb)
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


  nparam = length(par_names)

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

    dates_fit = mydata$data_fit$date
    inc_fit   = mydata$data_fit$inc

    data_fit = data.frame(x=dates_fit, y = inc_fit)

    cadence = as.numeric(dates[2]-dates[1])
    if (cadence == 1) cadence_lab = 'Daily'
    if (cadence == 7) cadence_lab = 'Weekly'

    main_txt = paste0(reg_name, ' - ', disease,' ',cadence_lab)

    start_year = lubridate::year(range(dates)[1])
    end_year   = lubridate::year(range(dates)[2])
    xlab = paste0(start_year,' - ', end_year)
    ylab = inc_type
    data = data.frame(x=dates, y = inc)



    pl[[ip]] <- ggplot() +
      geom_point(data = data_fit, aes(x,y), color = col[ip], alpha = 1.0, size = 3) +
      geom_point(data = data, aes(x,y), color=col[ip], alpha = 0.2, size = 3) +
      geom_vline(xintercept=max(data_fit$x), linetype = 'dashed', col = 'darkgreen') +
      labs(x = xlab,           # X-axis label
           y = ylab,           # Y-axis label
           title = main_txt)  # Main title

  }

  if (npath == 2) {
    suppressWarnings(print(grid.arrange(pl[[1]], pl[[2]], ncol = 2)))
    if (!is.null(filename)) {
      suppressWarnings(print(grid.arrange(pl[[1]], pl[[2]], ncol = 2)))
      ggsave(filename = filename, plot = grid_plots, width = 14, height = 6, dpi = 300)
      cat("\n Saving Fit Plots to: ", filename,'\n')
    }
  } else {
    suppressWarnings(pl[[1]])
    if (!is.null(filename)) {
      ggsave(filename = filename, plot = last_plot(), width = 7, height = 6, dpi = 300)
      cat("\n Saving Fit Plots to: ", filename,'\n')
    }

  }

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
      inc = inc_all[ind0:length(inc)]
    }

    simdat = simdat_list[[ip]]

    simdat = simdat[1:ntraj_both, ind0:ind1]

    simdat_list[[ip]] = simdat

    if (ip == 1) {
      rand_simdat_both = ordered_simdat_both = simdat * 0.0
      obs_both = inc * 0.0
    }

    rand_simdat_both = rand_simdat_both + simdat
    obs_both = obs_both + inc

    # now order also

    max_values <- apply(simdat, 1, max)
    # print(length(max_values))
    ordered_simdat <- simdat[order(max_values),]
    ordered_simdat_both = ordered_simdat_both + ordered_simdat

  }

  simdat_both = list()
  simdat_both[['random']]  = rand_simdat_both
  simdat_both[['ordered']] = ordered_simdat_both

  return(list(simdat_both = simdat_both, obs_both = obs_both, dates_both = dates_both))
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
#'
#'@return change in each compartment, real
#'
#'
td.seirh.ode <- Csnippet("
  double beta_cur;

  beta_cur = Beta1 + Beta2;
  beta_cur = beta_cur + (Beta2-Beta1) * tanh((time-tcng1)/wl);
  beta_cur = beta_cur * 0.5;

  DS = -beta_cur*S*I/N;
  DE = beta_cur*S*I/N - mu_EI*E;
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
  P = S + E + I + H1 + H2 + R;
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
#'
#'@return change in each compartment, integer
#'
#'
td.seirh.step <- "
  double P;
  double beta_cur;
  P = S + E + I + H1 + H2 + R;

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

