#'
#' Fit model(s) to dataset(s)
#'
#' Main driver function for the sequential fitting procedure of one or two
#' diseases
#'
#' @param prof_data data structure for diseases
#' for each disease it includes:
#' disease - covid19 or influenza
#' population - population size
#' loc_name - location name
#' inc_type - incidence type, e.g. hosp_admits
#' data - all available data as a 2D structure of dates and incidence
#' data_fit - subset of data for fitting (can be eqaul to data)
#' @param par_list data structure for parameters
#' for each disease it includes
#' model - 'seirh' or 'sirh'
#' constant_dis_pars - initial values for the compartments of the model
#' dis_par_ranges - a list with parameter ranges and starting values
#' mcmc_pars
#' @return
#' tab_list - posterior distribution of parameters for each disease
#' state0 - the initial conditions for all the compartments
#' wl - the parameter used for tanh of beta(t), default 3
#'
fit_data <- function(prof_data, par_list) {

  if (is.null(prof_data)) stop('Missing prof_data')
  if (is.null(par_list)) stop('Missing par_list')

  # number of diseases
  npath = length(prof_data)

  if (npath > 2 | npath < 1) stop('Incorrect Number of diseases found in prof_data')

  npath = length(par_list)

  if (npath > 2 | npath < 1) stop('Incorrect Number of diseases found in par_list')

  # prepare a list for the Posterior distribution of each disease fit
  # and for the initial conditions

  tab_list = state0_list = list()

  # loop on all diseases
  for (ip in 1:npath) {

    mydata = prof_data[[ip]]

    prof_init_par = par_list[[ip]]

    if (is.null(mydata)) stop('Incorrect Data Structure for prof_data')
    # location name
    reg_name = mydata$loc_name
    if (is.null(reg_name)) stop('Missing <loc_name> in prof_data structure')
    # location population
    pop = mydata$population
    if (is.null(pop)) stop('Missing <population> in prof_data structure')
    # disease name (covid or flu)
    disease = mydata$disease
    if (is.null(disease)) stop('Missing <disease> in prof_data structure')
    # hospitalization incidence - for fitting
    inc = mydata$data_fit$inc
    if (is.null(inc)) stop('Missing <inc> in prof_data structure')
    # dates - for fitting
    dates  = mydata$data_fit$date
    if (is.null(dates)) stop('Missing <date> in prof_data structure')

    cat("\nFitting ",toupper(disease), " Data for ", reg_name,'\n')

    ndates = length(dates)

    # using the date array build an integer day array

    times = dates_to_int(dates)

    ntimes = length(times)

    # retrieve model 'seirh' or 'sirh' based on disease

    model = prof_init_par$model # get_model(disease)

    # estimate the initial number of infections

    cat("\nEstimating Initial Number of Infectious ", '\n')
    I0est = est_I0(inc, disease)

    # based on model/disease set initial conditions for the states/compartments

    # but first check to see if the user has set it up

    if (any(is.na(prof_init_par$constant_dis_pars))) {
      state0 <- set_init_states(pop, I0est, model)
    } else {
      cat("Using User Provided prof_init_par$constant_dis_pars \n
          for Initial Number of Infectious\n")
      state0 <- prof_init_par$constant_dis_pars
    }


    # save the initial state in a list, it is needed for next steps

    state0_list[[disease]] <- state0

    # set some disease specific model parameters

    if (any(is.na(prof_init_par$dis_par_ranges$par))) {
      param0 <- init_param(model, inc)
      # estimate for baseline - will need to set key word based on number of weeks
      # of data

      end_of_season = FALSE

      baseline <- get_baseline(inc, end_of_season)

      # append 'baseline' to parameter list

      param0 <- append(param0, c('baseline' = baseline ))
    } else {
      param0 <- prof_init_par$dis_par_ranges$par
    }

    #
    # depending on covid or flu we and if the user has not provided an initial
    # guess for the parameters we try to get an improved initial guess using
    # optim in the cse of sirh. We may remove this since it often
    # gives unreasonable initial guess for mu_H1H2 which we over-ride below
    #

    # Note that update is done ONLY if the user has not provided
    # an initial guess.  We do not want to over-ride anything the User provides
    #
    if (model == 'sirh' & any(is.na(prof_init_par$dis_par_ranges$par))) {
      # construct a parameter list as needed by run_optim
      list1 <- param0[c('Beta', 'gamma','mu_H1H2', 'rho', 'pH','baseline')]
      list2 <- state0[c('S0', 'I0','R0')]
      param_optim <- c(list1, list2, 'pop' = pop)
      param_optim <- run_optim(param_optim, inc)

      # param0 will now inlcude the following
      # "Beta" "mu_H1H2"  "pH"  "gamma"  "pop" "S0" "I0"  "R0"  "rho"  "baseline"
      param0 <- c(param_optim[c('Beta', 'gamma','mu_H1H2', 'rho', 'pH','baseline')], param0['wl'])
      # caution!! will need to check for unreasonable parameter values
    } else {
      # construct a parameter list as needed by run_optim
      list1 <- param0[c('Beta', 'gamma','mu_H1H2', 'mu_EI', 'rho', 'pH','baseline')]
      list2 <- state0[c('S0', 'I0','R0')]
      # note here we do not call run_optim since it tends to fail
      param_optim <- c(list1, list2, 'pop' = pop)

      # param0 will now inlcude the following
      # "Beta" "mu_H1H2"  "pH"  "gamma"  "pop" "S0" "I0"  "R0"  "rho"  "baseline"
      param0 <- c(param_optim[c('Beta', 'gamma','mu_H1H2', 'mu_EI', 'rho', 'pH','baseline')], param0['wl'])
    }

    # Define all the states that will be integrated/accumulated
    # Define initial values for all the states
    if (model == 'sirh') {
      states <- c('S','I','R','H1','H2','Ic','Ih')
      init_states <- c(state0[c('S0','I0','R0')],
                       H10 = 0, H20 = 0, Ic0 = 0,Ih0 = 0)
    } else {
      states <- c('S','E','I','R','H1','H2','Ic','Ih')
      init_states <- c(state0[c('S0', 'E0','I0','R0')],
                       H10 = 0, H20 = 0, Ic0 = 0, Ih0 = 0)
    }

    nstates = length(states)

    # define time-dependent FOI
    #
    # We will need to work on this if we allow for more than two values
    nb = 2

    td_foi <- set_td_beta(nb, length(inc), param0$Beta)

    # param names is updated to include TD-FOI

    par_names = prof_init_par$dis_par_ranges$par_names


    param_info_list <- update_par_names(nb, par_names)

    nparam = param_info_list$nparam

    nparamtot = param_info_list$nparamtot

    par_names = param_info_list$par_names

    # We need to add and know the population

    par_names = c('pop', par_names)

    # need to add a test that sum(state0 == population)

    if (model == 'sirh') {
      param_sml = c(pop = pop, param0[c('gamma','pH','mu_H1H2', 'rho', 'baseline')])
    } else {
      param_sml = c(pop = pop, param0[c('gamma','pH','mu_H1H2', 'mu_EI','rho', 'baseline')])
    }

    par =list()

    par = c(param_sml, td_foi$beta, td_foi$tcng)

    # state variables that will be accumulated

    accum = c('Ic', 'Ih')
    iaccum = which(states %in% accum)
    naccum = length(accum)

    # imodel is Needed by Fortran code to select between SEIRH and SIRH models
    if (disease == 'covid19') {
      imodel = 1
      paropt = c('mu_H1H2', 'mu_EI', 'pH', 'baseline', names(td_foi$beta), names(td_foi$tcng[1:(nb-1)]))
    } else {
      imodel = 2
      paropt = c('mu_H1H2', 'pH', 'baseline', names(td_foi$beta), names(td_foi$tcng[1:(nb-1)]))
    }


    dt = 1./20.0

    t0 = 0

    accum = c('Ic', 'Ih')
    iaccum = which(states %in% accum)
    naccum = length(accum)

    # observed
    obs = inc

    # Gamma function of observed incidence

    gamaObs = lgamma(round(inc) + 1)

    # weight of each data point

    wght = rep(1.0, ntimes)

    nMCMC  = prof_init_par$mcmc_pars$nMCMC
    nlines = prof_init_par$mcmc_pars$nlines

    ithin = round(nMCMC/nlines)

    iseed =  as.numeric(Sys.time())

    # index  and number of parameters that will be optimized

    ind_opt = which(names(par) %in% paropt)

    nopt = length(ind_opt)

    input_parmin = prof_init_par$dis_par_ranges$parmin
    input_parmax = prof_init_par$dis_par_ranges$parmax

    parmin = par
    parmin[ind_opt] <- lapply(par[ind_opt], function(x) x * 0.5)
    # hand-tune some min values
    parmin['pH'] = 1e-4
    parmin[['mu_H1H2']] = 0.5
    parmin[['baseline']] = max(parmin[['baseline']], 1)
    # if user provided values use them
    if (!any(is.na(input_parmin))) {
      ind_opt_input_parmin = which(names(input_parmin) %in% paropt)
      parmin[ind_opt_input_parmin] = input_parmin[ind_opt_input_parmin]
    }

    parmax = par
    parmax[ind_opt] <- lapply(par[ind_opt], function(x) x * 2.0)
    # hand-tune some max values
    parmax['pH'] = 0.1
    parmax[['mu_H1H2']] = 2.0
    # if user provided values use them
    if (!any(is.na(input_parmax))) {
      ind_opt_input_parmax = which(names(input_parmax) %in% paropt)
      parmin[ind_opt_input_parmax] = input_parmin[ind_opt_input_parmax]
    }

    # sanity check - ensure that initial guesses for Beta/time of change are within
    # the allowed range

    for(my_ind in ind_opt ) {
      if (par[[my_ind]] >= parmax[[my_ind]] | par[[my_ind]] <= parmin[[my_ind]]) {
        par[[my_ind]] = parmin[[my_ind]] + 0.5*(parmax[[my_ind]] - parmin[[my_ind]])
      }
    }

    # Call to MCMC procedure happens here
    # total number of parameters

    # note that step and tab use nparam_tot

    step = rep(0.02, nparamtot)

    traj = array(0, c(ntimes, nstates))

    tab = array(0, c(nlines, (nparamtot + 1)))

    cat("\nStarting MCMC procedure..\n\n")

    start = Sys.time()

    out <- .Fortran('mcmc', nstates = as.integer(nstates), init = as.double(init_states), param = as.double(par),
                    nparam = as.integer(nparam), nb = as.integer(nb), time = as.double(times), ntimes = as.integer(ntimes),
                    t0 = as.double(t0), dt = as.double(dt), traj = as.double(traj),
                    naccum = as.integer(naccum), iaccum = as.integer(iaccum),parmin = as.double(parmin),
                    parmax=as.double(parmax), step = as.double(step), wl = as.double(param0$wl),
                    ind_opt = as.integer(ind_opt), nopt = as.integer(nopt),
                    obs = as.double(obs), gamaObs = as.double(gamaObs), wght = as.double(wght), nMCMC = as.integer(nMCMC),
                    ithin =as.integer(ithin), iseed = as.integer(iseed), tab = as.single(tab), imodel = as.integer(imodel))

    end = Sys.time()
    # print(end-start)

    # mainly for debug purposes

    traj = array(out$traj, c(ntimes, nstates))

    colnames(traj) <- states

    traj = data.frame(traj)

    param_best = out$param

    names(param_best) <- par_names

    traj[,'cases'] = rpois(ntimes, traj[,'Ih'] * param_best['rho'] + param_best['baseline'])

    traj[,'obs'] = obs

    tab = array(out$tab, c(nlines, (nparamtot + 1)))

    colnames(tab) <- c(par_names, 'llk')

    tab_list[[disease]] <- tab
  }

  # Will also need to SAVE all what we are returning

  return(list(tab_list = tab_list, state0_list = state0_list, wl = param0$wl))

}


