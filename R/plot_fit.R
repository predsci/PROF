#'
#' Plots the Results of Fit model(s) to dataset(s) and Results of a Statistical
#' Baseline Model
#'
#' For the Mechanistic Results, uses the posterior distribution of the fit(s) and
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
#'
#'  tab_list - a list with the posterior distribution(s) for disease(s) fit(s)
#'  for each disease it includes the population, model parameters and LLK
#'  state0_list - A list with initial number of individuals in each compartment
#'  for each pathogen
#'  wl real, needed for tanh function of bete(t)
#'  @filename - if NULL print plot to screen, if not also save to filename
#'  @param ntraj - integer number of stochastic trajectories, default 1000
#' @return
#' plots to screen or file and a list with the following items
#'
#' fit_traj - a list for each disease containing: model fit mechanistic trajectories,
#' dates, and reported incidence
#'
#' stat_traj - a list for each disease containing: baseline statistical trajectories,
#' dates and reported incidence
#'
#' pl - a list of ggplot2 objects one for each disease for the mechanistic plots
#'
#' pl_stat - a list of ggplot2 objects one for each disease for the statistical plots
#'
#'
plot_fit <- function(prof_data, par_list, fit_list, ntraj =1000, filename = NULL) {

  tab_list = fit_list$tab_list

  state0_list = fit_list$state0_list

  wl = fit_list$wl

  npath = length(prof_data)

  disease_list = names(prof_data)

  pl = pl_stat = list()
  fit_traj = stat_traj = list()

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

    simdat <- array(0, c(ntraj, ntimes))

    # retrieve posterior for disease

    tab = tab_list[[disease]]

    nlines = dim(tab)[1]

    nparamtot = dim(tab)[2] -1 # it includes the LLK hence the -1

    z <- round(nlines*2/3):nlines
    ind <- sample(z, round( 1.2* ntraj))

    # retrieve initial conditions for disease

    state0 = state0_list[[disease]]

    # set the model to 'sirh' or 'seirh' based on disease

    model = prof_init_par$model

    # observations - fitted

    obs = mydata$data_fit$inc

    # print information for the User

    cat("\nCreating Fit Plots For", reg_name, ' ', toupper(disease),'\n')

    if (model == 'sirh') {
      # Here using a time-dependent version
      parnames_td_sirh = c("Beta1","Beta2", "tcng1","wl","gamma","mu_H1H2",
                           "pop","pH","S0",'I0',"R0", "rho","baseline")

      pomp(data=data.frame(time = 1:ntimes),
           times="time",t0=0,
           rprocess=euler(step.fun = Csnippet(td.sirh.step),delta.t=1/40.),
           accumvars = c("Ic", "Ih"),
           rinit=td.init,
           rmeasure=rmeas,
           dmeasure=dmeas,
           obsnames="cases",
           statenames=c("S","I","R","H1","H2","Ic","Ih","time"),
           paramnames=parnames_td_sirh) -> flu_sirh


      icount=0

      for (ii in ind) {

        mypar <- tab[ii, 1:nparamtot]
        state0["I0"] = mypar['I0']

        coef(flu_sirh) <- c(mypar['Beta1'], mypar['Beta2'], mypar['tcng1'], mypar['gamma'],
                        mypar['mu_H1H2'], mypar['pH'], mypar['pop'],
                        state0["I0"], state0["S0"], state0["R0"],
                        mypar['rho'], round(mypar['baseline']), wl = wl)
        # generate simulation data with the parameters defined above

        model.pred <- simulate(flu_sirh, format="data.frame", nsim = 1)
        if (model.pred$cases[which.max(obs)] > round(mypar['baseline'])) {
          icount = icount + 1
          simdat[icount,] <- model.pred$cases
        }

        if (icount == ntraj) break

      }

      simdat = simdat[1:icount,]
      # Something new - create also a statistical fit to the pathogen time-series

      simdat_stat = stat_fit(obs, ntraj)


    } else {

      parnames_td_seirh = c("Beta1","Beta2", "tcng1","wl","gamma","mu_H1H2","mu_EI", "pop","pH","S0","E0",'I0',"R0", "rho","baseline")

      pomp(data=data.frame(time = 1:ntimes),
           times="time",t0=0,
           rprocess=euler(step.fun = Csnippet(td.seirh.step),delta.t=1/40.),
           accumvars = c("Ic", "Ih"),
           rinit=td.init.seirh,
           rmeasure=rmeas,
           dmeasure=dmeas,
           obsnames="cases",
           statenames=c("S","E","I","R","H1","H2","Ic","Ih", 'time'),
           paramnames=parnames_td_seirh) -> covid_seir

      icount=0

      for (ii in ind) {

        mypar <- tab[ii, 1:nparamtot]

        state0["I0"] = mypar['I0']
        coef(covid_seir) <- c(mypar['Beta1'], mypar['Beta2'], mypar['tcng1'], mypar['gamma'],
                         mypar['mu_H1H2'], mypar['mu_EI'], mypar['pH'], mypar['pop'],
                         state0['I0'], state0['S0'], state0['E0'], state0['R0'],
                         mypar['rho'], round(mypar['baseline']), wl = wl)

        # generate simulation data with the parameters defined above

        model.pred <- simulate(covid_seir, format="data.frame", nsim = 1)
        if (max(model.pred$cases) > mypar['baseline']){
          icount = icount + 1
          simdat[icount,] <- model.pred$cases
         }

        if (icount == ntraj) break

      }

      simdat = simdat[1:icount,]
      # Something new - create also a statistical fit to the pathogen time-series

      simdat_stat = stat_fit(obs, ntraj)

    }

    # save to list

    fit_traj[[disease]] = list(traj = simdat,
                               date = as.Date(dates, format = '%Y-%m-%d'),
                               reported = obs)

    apply(simdat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles

    quantiles <- t(quantiles)
    quantiles <- as.data.frame(quantiles)
    total=cbind(date = as.Date(dates, format = '%Y-%m-%d'),time = 1:ntimes,quantiles,
                reported = obs)

    total = as.data.frame(total)

    cadence = as.numeric(dates[2]-dates[1])
    if (cadence == 1) cadence_lab = 'Daily'
    if (cadence == 7) cadence_lab = 'Weekly'

    ylab = paste0(cadence_lab, ' New Hosp')

    title = paste0(reg_name,' - ', toupper(disease),' Mechanistic Fit')

    start_year = lubridate::year(range(dates)[1])
    end_year   = start_year + 1
    xlab = paste0(start_year,' - ', end_year)

    pl[[disease]] <- ggplot(data=total,
                         mapping=aes(x=date))+
      geom_line(aes(y=`50%`),color='red')+
      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='red',alpha=0.2)+
      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill='red',alpha=0.4)+
      geom_point(aes(y=reported),color='black')+
      labs(y=ylab,x=xlab) + ggtitle(title)


    stat_traj[[disease]] = list(traj = simdat_stat,
                                date = as.Date(dates, format = '%Y-%m-%d'),
                                reported = obs)

    apply(simdat_stat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles_stat

    quantiles_stat <- t(quantiles_stat)
    quantiles_stat <- as.data.frame(quantiles_stat)
    total_stat=cbind(date = as.Date(dates, format = '%Y-%m-%d'),time = 1:ntimes,quantiles_stat,
                reported = obs)

    title = paste0(reg_name,' - ', toupper(disease),' Statistical Baseline Model')

    pl_stat[[disease]] = ggplot(data=total_stat,
                                           mapping=aes(x=date))+
      geom_line(aes(y=`50%`),color='red')+
      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='red',alpha=0.2)+
      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill='red',alpha=0.4)+
      geom_point(aes(y=reported),color='black')+
      labs(y=ylab,x=xlab) + ggtitle(title)

  } #end of loop over diseases

  cat("\nMaking Plots\n\n")

  if (npath == 2) {
    suppressWarnings(print(grid.arrange(pl[[1]],  pl[[2]], pl_stat[[1]], pl_stat[[2]], ncol = 2)))
    if (!is.null(filename)) {
      suppressWarnings(print(grid.arrange(pl[[1]],  pl[[2]], pl_stat[[1]], pl_stat[[2]], ncol = 2)))
      ggsave(filename = filename, plot = grid_plots, width = 14, height = 10, dpi = 300)
      cat("\n Saving Fit Plots to: ", filename,'\n')
    }
  } else {
    suppressWarnings(print(grid.arrange(pl[[1]], pl_stat[[1]], ncol = 2)))
    if (!is.null(filename)) {
      suppressWarnings(print(grid.arrange(pl[[1]], pl_stat[[1]], ncol = 2)))
      ggsave(filename = filename, plot = grid_plots, width = 14, height = 6, dpi = 300)
      cat("\n Saving Fit Plots to: ", filename,'\n')
    }

  }

  # return the trajectory list

  return(list(fit_traj = fit_traj, stat_traj = stat_traj, pl = pl, pl_stat = pl_stat))

}
