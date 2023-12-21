#'
#' Plots the Results of Fit model(s) to dataset(s)
#'
#' Uses the posterior distribution of the fit(s) and
#' a stochastic code to generate trajectories and calculate
#' the statistics
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
#' model
#' constant_dis_pars
#' dis_par_ranges
#' mcmc_pars
#' @param fit_list - output of fit_data. A list with the following components
#'  tab_list - a list with the posterior distribution(s) for disease(s) fit(s)
#'  for each disease it includes the population, model parameters and LLK
#'  state0_list - A list with initial number of individuals in each compartment
#'  for each pathogen
#'  wl real, needed for tanh function of bete(t)
#'  @param ntraj - integer number of stochastic trajectories, default 1000
#'  @param nfrcst - number of time units to produce a forecast for (assume to be the same
#'  cadence as the data), default is 35 days
#'  @filename - if NULL print plot to screen, if not save also to filename
#' @return
#' plots to screen and  file
#' forecast_traj - a list with a list for each disease and for the combined burden
#' (random and ordered): trajectories,
#' dates, and reported incidence.
#'
plot_forecast <- function(prof_data, par_list, fit_list, ntraj =1000, nfrcst = 35, filename = NULL) {

  tab_list = fit_list$tab_list

  state0_list = fit_list$state0_list

  wl = fit_list$wl

  nb_vec = fit_list$nb_vec

  npath = length(prof_data)

  disease_list = names(prof_data)

  pl = simdat_list = dates_frcst_list = list()

  forecast_traj = list()

  # loop on all diseases
  for (ip in 1:npath) {

    mydata = prof_data[[ip]]

    prof_init_par = par_list[[ip]]

    # location name
    reg_name = mydata$loc_name

    # location population
    pop = mydata$population

    # disease name (covid or flu)
    disease = mydata$disease

    # hospitalization incidence - fitted
    inc = mydata$data_fit$inc

    # dates - fitted
    dates  = mydata$data_fit$date

    ndates = length(dates)

    # using the date array build an integer day array

    times = dates_to_int(dates)

    ntimes = length(times)

    ntimes_frcst= ndates + nfrcst

    # build also the arrays for the forecasts

    cadence = as.numeric(dates[2]-dates[1])
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


    dates_frcst_list[[ip]] = dates_frcst

    # retrieve posterior for disease

    tab = tab_list[[disease]]

    nlines = dim(tab)[1]

    nparamtot = dim(tab)[2] -1 # it includes the LLK hence the -1

    z <- round(nlines*2/3):nlines

    ind <- sample(z, round( 1.2* ntraj))

    ind <- order(tab[,'llk'])

    simdat <- array(0, c(ntraj, ntimes_frcst))

    # retrieve initial conditions for disease

    state0 = state0_list[[disease]]

    # set the model to 'sirh' or 'seirh' based on disease

    model = prof_init_par$model

    # observations - all data stream

    obs = mydata$data$inc

    obs_fit = mydata$data_fit$inc

    # number of values for FOI

    nb = nb_vec[ip]

    #print information to the User
    cat("\nCreating Forecast: ", nfrcst," ", print_lab, " Forward for ", reg_name, ' ', toupper(disease),'\n')

    if (model == 'sirh') {
      # Here using a time-dependent version
      # parnames_td_sirh = c("Beta1","Beta2", "tcng1","wl","gamma","mu_H1H2",
      #                      "pop","pH","S0",'I0',"R0", "rho","baseline")
      #
      # pomp(data=data.frame(time = 1:ntimes_frcst),
      #      times="time",t0=0,
      #      rprocess=euler(step.fun = Csnippet(td.sirh.step),delta.t=1/40.),
      #      accumvars = c("Ic", "Ih"),
      #      rinit=td.init,
      #      rmeasure=rmeas,
      #      dmeasure=dmeas,
      #      obsnames="cases",
      #      statenames=c("S","I","R","H1","H2","Ic","Ih","time"),
      #      paramnames=parnames_td_sirh) -> flu_sirh


      icount=0

      for (ii in ind) {

        mypar <- tab[ii, 1:nparamtot]
        state0["I0"] = mypar['I0']
        state0$S0 = as.numeric(mypar['pop']) - state0$I0

        # coef(flu_sirh) <- c(mypar['Beta1'], mypar['Beta2'], mypar['tcng1'], mypar['gamma'],
        #                     mypar['mu_H1H2'], mypar['pH'], mypar['pop'],
        #                     state0["I0"], state0["S0"], state0["R0"],
        #                     mypar['rho'], round(mypar['baseline']), wl = wl)
        # # generate simulation data with the parameters defined above
        #
        # model.pred <- simulate(flu_sirh, format="data.frame", nsim = 1)
        #
        # if (model.pred$cases[which.max(obs)] > round(mypar['baseline']) *2){
        #   icount = icount + 1
        #   simdat[icount,] <- model.pred$cases
        # }

        yinit = c(state0$S0, state0$I0, state0$E0, 0, 0, 0)
        parms = c(mypar, 'wl' =wl)

        if (nb == 2) {
          results <- ode(y=yinit, t = 1:ntimes_frcst, method='lsoda', func=td2_sirh_dynamics, parms = parms)
        } else {
          results <- ode(y=yinit, t = 1:ntimes_frcst, method='lsoda', func=td3_sirh_dynamics, parms = parms)
          # calling H2 Ih here
        }

        model.pred = results[,-1] # remove the time column

        # generate simulation data with the parameters defined above

        Ih = model.pred[,4]
        cases <- rpois(ntimes_frcst, Ih * mypar[['rho']] + mypar[['baseline']])

        # if (model.pred$cases[which.max(obs)] > round(mypar['baseline'])) {
        if (cases[which.max(obs)] > round(mypar['baseline'])) {
          icount = icount + 1
          simdat[icount,] <- cases
        }

        if (icount == ntraj) break

      }

    } else {

      # parnames_td_seirh = c("Beta1","Beta2", "tcng1","wl","gamma","mu_H1H2","mu_EI", "pop","pH","S0","E0",'I0',"R0", "rho","baseline")
      #
      # pomp(data=data.frame(time = 1:ntimes_frcst),
      #      times="time",t0=0,
      #      rprocess=euler(step.fun = Csnippet(td.seirh.step),delta.t=1/40.),
      #      accumvars = c("Ic", "Ih"),
      #      rinit=td.init.seirh,
      #      rmeasure=rmeas,
      #      dmeasure=dmeas,
      #      obsnames="cases",
      #      statenames=c("S","E","I","R","H1","H2","Ic","Ih", 'time'),
      #      paramnames=parnames_td_seirh) -> covid_seir

      icount=0

      for (ii in ind) {

        mypar <- tab[ii, 1:nparamtot]

        state0["I0"] = mypar['I0']
        state0["E0"] = mypar['I0']
        state0$S0 = as.numeric(mypar['pop']) - (state0$I0+state0$E0)

        # coef(covid_seir) <- c(mypar['Beta1'], mypar['Beta2'], mypar['tcng1'], mypar['gamma'],
        #                       mypar['mu_H1H2'], mypar['mu_EI'], mypar['pH'], mypar['pop'],
        #                       state0['I0'], state0['S0'], state0['E0'], state0['R0'],
        #                       mypar['rho'], round(mypar['baseline']), wl = wl)
        #
        # # generate simulation data with the parameters defined above
        #
        # model.pred <- simulate(covid_seir, format="data.frame", nsim = 1)
        #
        # if (max(model.pred$cases) > mypar['baseline']){
        #   icount = icount + 1
        #   simdat[icount,] <- model.pred$cases
        #
        # }

        yinit = c(state0$S0, state0$I0, state0$E0, 0, 0, 0)
        parms = c(mypar, 'wl' =wl)

        if (nb == 2) {
          results <- ode(y=yinit, t = 1:ntimes_frcst, method='lsoda', func=td2_seirh_dynamics, parms = parms)
        } else {
          results <- ode(y=yinit, t = 1:ntimes_frcst, method='lsoda', func=td3_seirh_dynamics, parms = parms)
          # calling H2 Ih here
        }

        model.pred = results[,-1] # remove the time column

        # generate simulation data with the parameters defined above

        Ih = model.pred[,5]
        cases <- rpois(ntimes_frcst, Ih * mypar[['rho']] + mypar[['baseline']])

        if (max(cases) > mypar['baseline']){
          icount = icount + 1
          simdat[icount,] <- cases
        }

        if (icount == ntraj) break

      }

    }

    simdat = simdat[1:icount,]

    simdat_list[[ip]] = simdat

    npad = nfrcst - length(obs)
    if (npad > 0) {
      reported = c(obs[1:length(dates_frcst)], rep(NA, npad))
    } else {
      reported = obs[1:length(dates_frcst)]
    }

    npad_fit = nfrcst - length(obs_fit)

    if (npad_fit > 0) {
      reported_fit = c(obs_fit[1:length(dates_frcst)], rep(NA, npad_fit))
    } else {
      reported_fit = obs_fit[1:length(dates_frcst)]
    }

    forecast_traj[[disease]] = list(traj = simdat,
                                    date = as.Date(dates_frcst, format = '%Y-%m-%d'),
                                    reported = reported, reported_fit = reported_fit)

    apply(simdat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles

    quantiles <- t(quantiles)
    quantiles <- as.data.frame(quantiles)
    total=cbind(date = as.Date(dates_frcst, format = '%Y-%m-%d'),time = 1:ntimes_frcst,quantiles,
                reported = reported, reported_fit = reported_fit)

    # Remove 'X' from column names
    #colnames(total) <- gsub("X", "", colnames(total))

    total = as.data.frame(total)

    if (cadence == 1) cadence_lab = 'Daily'
    if (cadence == 7) cadence_lab = 'Weekly'

    ylab = paste0(cadence_lab, ' New Hosp')

    title = paste0(reg_name,' - ', toupper(disease))

    start_year = lubridate::year(range(dates)[1])
    end_year   = lubridate::year(range(dates)[2])
    xlab = paste0(start_year,' - ', end_year)

    pl[[disease]] <- suppressMessages(ggplot(data=total,
                             mapping=aes(x=date))+
      geom_line(aes(y=`50%`),color='red')+
      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='red',alpha=0.2)+
      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill='red',alpha=0.4)+
      geom_point(aes(y=reported),color='black', alpha = 0.4)+
      geom_point(aes(y=reported_fit),color='black', alpha = 1.)+
      geom_vline(xintercept = dates[ntimes], linetype = "dashed", color = "cornflowerblue", size = 1.5) +
      labs(y=ylab,x=xlab) + ggtitle(title))

  } #end of loop over diseases


  # Combine forecasts
  cat("Combining Forecasts \n")

  combined_frcst <- combine_forecasts(prof_data, dates_frcst_list, simdat_list)

  simdat_both = combined_frcst$simdat_both

  dates_both  = combined_frcst$dates_both

  obs_both    = combined_frcst$obs_both

  obs_fit_both = combined_frcst$obs_fit_both

  npad = nfrcst - length(obs_both)
  if (npad > 0) {
    reported_both = c(obs_both[1:length(dates_frcst)], rep(NA, npad))
  } else {
    reported_both = obs_both[1:length(dates_frcst)]
  }

  npad_fit = nfrcst - length(obs_fit_both)
  if (npad_fit > 0) {
    reported_fit_both = c(obs_fit_both[1:length(dates_frcst)], rep(NA, npad))
  } else {
    reported_fit_both = obs_fit_both[1:length(dates_frcst)]
  }

  combined_names <- c('random', 'sorted')

  # find maximum in quantiles_both of random and sorted and use

  both_max = 0
  quantiles_both_list = list()
  for (ip in 1:npath) {
    apply(simdat_both[[ip]],2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles_both
    quantiles_both <- t(quantiles_both)
    quantiles_both <- as.data.frame(quantiles_both)
    both_max = max(both_max, round(max(quantiles_both)))
    quantiles_both_list[[combined_names[[ip]]]] = quantiles_both
  }

  for (ip in 1:npath) {

  # apply(simdat_both[[ip]],2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles_both
  #
  # quantiles_both <- t(quantiles_both)
  # quantiles_both <- as.data.frame(quantiles_both)

    quantiles_both <- quantiles_both_list[[combined_names[[ip]]]]
  forecast_traj[[combined_names[[ip]]]] = list(traj = simdat_both[[ip]],
                                               date = as.Date(dates_both, format = '%Y-%m-%d'),
                                               reported_both = reported_both,
                                               reported_fit_both = reported_fit_both)

  total_both=cbind(date = as.Date(dates_both, format = '%Y-%m-%d'),quantiles_both,
              reported = c(obs_both, rep(NA, length(dates_both)-length(obs_both))),
              reported_fit = c(obs_fit_both, rep(NA, length(dates_both)-length(obs_fit_both))))

  title_both = paste0(reg_name,' - Combined Burden (', combined_names[ip],')')

  pl[[combined_names[ip]]] <- suppressMessages(ggplot(data=total_both,
                                           mapping=aes(x=date))+
                                      geom_line(aes(y=`50%`),color='red')+
                                      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='red',alpha=0.2)+
                                      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill='red',alpha=0.4)+
                                      geom_point(aes(y=reported),color='black', alpha = 0.4)+
                                      geom_point(aes(y=reported_fit),color='black', alpha = 1.)+
                                      geom_vline(xintercept = dates[ntimes], linetype = "dashed", color = "cornflowerblue", size = 1.5) +
                                      labs(y=ylab,x=xlab) + ggtitle(title_both)) + coord_cartesian(ylim = c(0, both_max))

  }

  interactive_plot <- list()

  for (ip in 1:npath) {
    interactive_plot[[ip]] <- ggplotly(pl[[ip]])
  }

  cat("\nMaking Plots\n\n")
  if (npath == 2) {

    suppressWarnings(print(grid.arrange(pl[[1]], pl[[2]], pl[[3]], pl[[4]], ncol = 2)))

    if (!is.null(filename)) {
      suppressWarnings(grid_plots <- grid.arrange(pl[[1]], pl[[2]], pl[[3]], pl[[4]], ncol = 2))
      ggsave(filename = filename, plot = grid_plots, width = 14, height = 6, dpi = 300)
      cat("\n Saving Forecast Plots to: ", filename,'\n')
    }
  } else {

    suppressWarnings(print(pl[[1]]))

    if (!is.null(filename)) {
      ggsave(filename = filename, plot = last_plot(), width = 7, height = 6, dpi = 300)
      cat("\n Saving Forecast Plots to: ", filename,'\n')
    }

  }

  # return forecast_traj

  return(forecast_traj)
}
