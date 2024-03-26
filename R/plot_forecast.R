#'
#' @title Plots Forecasts for Mechanistic Models
#'
#' @description Uses the posterior distribution of the fit(s) and
#' a stochastic code to generate trajectories, with the forecast horizon determined
#' by the call and calculate the statistics
#'
#' @import ggplot2
#' @import gridExtra
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
#'  @param filename - if NULL print plot to screen, if not save also to filename
#'  @param err_cor numeric scalar [-1, 1] - specify the error correlation to be used
#'  for combining influenza and COVID19 forecasts.  If NULL, default behavior is
#'  to evaluate for err_cor=1 (ordered) and err_cor=0 (random).
#'  @param method_name character - method used to aggregate pathogen forecasts 
#'  'semi_sorted_randA' or 'lin_scale'. 'semi_sorted_randA' 
#' is a more proper combination of profiles.  'lin_scale' uses the uncertainty 
#' range of 'semi_sorted_randA', but ensures that the combined forecast has a 
#' median equal to the sum of the aggregate forecast medians.
#' @return
#' plots to screen and  file
#' forecast_traj - a list with a list for each disease and for the combined burden
#' (random and ordered): trajectories,
#' dates, and reported incidence.
#' total_list - a list with the data frames used for each of the plots, these include the median and quantiles
#' wis_df - a list of data frame with the WIS score of the forecast if available.  If not it contains a NULL
#'
plot_forecast <- function(prof_data, par_list, fit_list, ntraj =1000, nfrcst = 35, filename = NULL, err_cor = 1.0, method_name="semi_sorted_randA") {

  tab_list = fit_list$tab_list

  state0_list = fit_list$state0_list

  wl = fit_list$wl

  nb_vec = fit_list$nb_vec

  npath = length(prof_data)

  disease_list = names(prof_data)

  pl = simdat_list = dates_frcst_list = total_list = list()

  forecast_traj = list()

  # https://www.statology.org/ggplot-default-colors/#:~:text=By%20default%2C%20ggplot2%20chooses%20to,and%20blue%20for%20the%20bars.&text=Here's%20how%20to%20interpret%20the,in%20the%20plot%20is%20%2300BA38.

  # default_colors <- c("#F8766D", "#00BFC4") #c('#F8766D','#619CFF','#00BA38','#FF00FF ')

  # Define colors for plots
  mycolor_list <- list('covid19' = "#F8766D", 'influenza'= "#00BFC4",
                       'combined' = "#CC79A7") #= "#D55E00",

  # Function to add transparency to colors
  add_transparency <- function(color, alpha) {
    # Convert hexadecimal color code to RGB format
    rgb_vals <- col2rgb(color) / 255

    # Append alpha value
    rgb_vals <- c(rgb_vals, alpha)

    # Convert back to hexadecimal format
    rgba_color <- rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], alpha = rgb_vals[4])

    return(rgba_color)
  }

  # Transparency values (adjust as needed)
  alpha_values <- c(0.5, 0.5, 0.8)

  mycolor_list_with_transparency <- Map(add_transparency, mycolor_list, alpha_values)

  reported_list = reported_fit_list = list()

  # loop on all diseases

  wis_df = list()

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
    dates_fit  = mydata$data_fit$date

    ndates = length(dates_fit)

    # using the date array build an integer day array

    times = dates_to_int(dates_fit)

    ntimes = length(times)

    ntimes_frcst= ndates + nfrcst

    # build also the arrays for the forecasts

    cadence = as.numeric(dates_fit[2]-dates_fit[1])
    if (cadence == 1) {
      cadence_lab = paste0(cadence, ' day')
      print_lab = 'Days'
      dates_frcst = seq(from = dates_fit[1], length = ntimes_frcst, by = '1 day')
    }

    if (cadence == 7) {
      cadence_lab = paste0(cadence, ' week')
      print_lab = 'Weeks'
      dates_frcst = seq(from = dates_fit[1], length = ntimes_frcst, by = '1 week')
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

    dates = mydata$data$date

    # start date of obs may not be the same as start date of obs_fit

    keep_ind = which(dates >= dates_fit[1])

    # trim
    obs = obs[keep_ind]
    dates = dates[keep_ind]

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
      #      accumvars = c("Ih"),
      #      rinit=td.init,
      #      rmeasure=rmeas,
      #      dmeasure=dmeas,
      #      obsnames="cases",
      #      statenames=c("S","I","R","H1","H2","Ih","time"),
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

        yinit = c(state0$S0, state0$I0, 0, 0, 0, 0)
        parms = c(mypar, 'wl' =wl)
        time0 = parms['time0']
        results0 <- ode(y=yinit, t = seq(from=0,to=time0, length=max(round(time0),5)), method = 'lsoda', func=td_sirh_dynamics, parms = parms)
        results0 <- results0[,-1]
        yinit0 <- as.numeric(results0[nrow(results0),])
        if (nb == 2) {
          results <- ode(y=yinit0, t = 1:ntimes_frcst, method='lsoda', func=td2_sirh_dynamics, parms = parms)
        } else {
          results <- ode(y=yinit0, t = 1:ntimes_frcst, method='lsoda', func=td3_sirh_dynamics, parms = parms)
        }

        model.pred = results[,-1] # remove the time column
        colnames(model.pred) = c('S', 'I', 'R', 'H1', 'H2', 'Ih')
        # generate simulation data with the parameters defined above

        Ih = c(0, diff(model.pred[,'Ih']))
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
      #      accumvars = c("Ih"),
      #      rinit=td.init.seirh,
      #      rmeasure=rmeas,
      #      dmeasure=dmeas,
      #      obsnames="cases",
      #      statenames=c("S","E","I","R","H1","H2","Ih", 'time'),
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

        yinit = c(state0$S0, state0$I0, state0$E0, 0, 0, 0, 0)
        parms = c(mypar, 'wl' =wl)
        time0 = parms['time0']
        results0 <- ode(y=yinit, t = seq(from=0,to=time0, length=max(round(time0),5)), method = 'rk4', func=td_seirh_dynamics, parms = parms)
        results0 <- results0[,-1]
        yinit0 <- as.numeric(results0[nrow(results0),])

        if (nb == 2) {
          results <- ode(y=yinit0, t = 1:ntimes_frcst, method='lsoda', func=td2_seirh_dynamics, parms = parms)
        } else {
          results <- ode(y=yinit0, t = 1:ntimes_frcst, method='lsoda', func=td3_seirh_dynamics, parms = parms)
        }

        model.pred = results[,-1] # remove the time column
        colnames(model.pred) = c('S', 'E', 'I', 'R', 'H1', 'H2', 'Ih')
        # generate simulation data with the parameters defined above

        Ih = c(0, diff(model.pred[,'Ih']))
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

    # score the forecast if possible


    if (length(dates) > length(dates_fit)) {
      dates_frcst_only = anti_join(data.frame(date=dates_frcst), data.frame(date=dates_fit), by = "date")$date #Forecast only dates
      # which of the forecast only dates are also in the observation dates
      dates_frcst_only_in_obs = semi_join(data.frame(date=dates_frcst_only), data.frame(date=dates), by = "date")$date
      keep_ind_obs = match(dates_frcst_only_in_obs, dates)
      keep_ind_model = match(dates_frcst_only_in_obs, dates_frcst)
      nscore = length(dates_frcst_only_in_obs)
      obs_score = obs[keep_ind_obs]
      dates_score = dates[keep_ind_obs]

      # find the subset from the model that we are going to score
      sim_score = simdat[, keep_ind_model]

      wis_arr = score_forecast(obs = obs_score, simdat = sim_score)

      wis_df[[disease]] = data.frame(date=dates_score, wis = wis_arr, disease = disease, model = 'mech')

    }

    npad = nfrcst - length(obs)
    if (npad > 0) {
      reported = c(obs, rep(NA, npad))
    } else {
      reported = obs[1:length(dates_frcst)]
    }

    npad_fit = nfrcst - length(obs_fit)

    if (npad_fit > 0) {
      reported_fit = c(obs_fit, rep(NA, npad_fit))
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

    total = as.data.frame(total)

    total_list[[disease]] = total

    reported_list[[disease]] = reported
    reported_fit_list[[disease]] = reported_fit

    copy_total = total

    total[1:length(obs_fit), c('2.5%','25%','50%','75%','97.5%')] <- NA

    # Remove 'X' from column names
    #colnames(total) <- gsub("X", "", colnames(total))

    total = as.data.frame(total)

    if (cadence == 1) cadence_lab = 'Daily'
    if (cadence == 7) cadence_lab = 'Weekly'

    ylab = paste0(cadence_lab, ' New Hosp')

    mycolor = mycolor_list_with_transparency[[disease]]
    mytitle = paste0(reg_name,' - ', toupper(disease))

    start_year = lubridate::year(range(dates)[1])
    end_year   = lubridate::year(range(dates)[2])
    xlab = paste0(start_year,' - ', end_year)

    pl[[disease]] <- ggplot(data=total,aes(x=date))+
      geom_col(aes(y=reported), fill = mycolor, alpha = 0.5) +
      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='blue',alpha=0.5)+
      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill='blue',alpha=0.8)+
      geom_line(aes(y=`50%`),color='black')+
      labs(y=ylab,x=xlab) +
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.7), legend.position = "none") +
      annotate("text", x = median(total$date), y = 0.93*max(total[,c('reported',"97.5%")], na.rm=TRUE), label = mytitle, size = 4)


  } #end of loop over diseases

  interactive_plot <- list()

  if (length(wis_df) != 0) {
    long_df = bind_rows(wis_df)
  } else {
    long_df = NULL
  }

  if (npath == 1) {

    suppressWarnings(print(ggplotly(pl[[1]])))

    if (!is.null(filename)) {
      ggsave(filename = filename, plot = last_plot(), width = 7, height = 6, dpi = 300)
      cat("\n Saving Forecast Plots to: ", filename,'\n')
    }
    return(list(arrange_plot = pl[[1]], total_list = total_list, wis_df = long_df))
  }

  # Combine forecasts
  cat("Combining Forecasts \n")
  
  # if (is.null(err_cor)) {
  #   combined_frcst <- combine_forecasts(prof_data, dates_frcst_list, simdat_list)
  # } else {
    combined_frcst_ecor <- combine_fore_err_corr(prof_data, dates_frcst_list, simdat_list,
                                                 err_corr=err_cor, nfrcst=nfrcst,
                                                 method_name=method_name)
    combined_frcst_rand <- combine_fore_err_corr(prof_data, 
                                                 dates_frcst_list, simdat_list,
                                                 err_corr=0, nfrcst=nfrcst,
                                                 method_name=method_name)
    
    combined_frcst = combined_frcst_ecor
    combined_frcst$simdat_both[[2]] = combined_frcst_rand$simdat_both[[1]]
    
    combined_names <- c("err_cor", "random")
    names(combined_frcst$simdat_both) = combined_names
    
  # }
  
  obs_each_list = combined_frcst$obs_each_list

  obs_fit_each_list = combined_frcst$obs_fit_each_list

  simdat_both = combined_frcst$simdat_both

  dates_both_data  = combined_frcst$dates_both_data
  dates_fore = combined_frcst$dates_fore

  obs_both    = combined_frcst$obs_both

  obs_fit_both = combined_frcst$obs_fit_both

  # npad = length(dates_both) - length(obs_both)
  npad = length(dates_fore)

  if (npad > 0) {
    reported_both = c(obs_both, rep(NA, npad))
  } else {
    reported_both = obs_both[1:length(dates_both_data)]
  }

  npad_fit = length(dates_both_data)

  if (npad_fit > 0) {
    reported_fit_both = c(obs_fit_both, rep(NA, npad))
  } else {
    reported_fit_both = obs_fit_both[1:length(dates_both_data)]
  }

  # find maximum in simdat_both of random and custom and use

  both_max = max(reported_both, na.rm=T)
  for (ic in 1:length(combined_names)) {
    both_max = max(both_max, round(max(simdat_both[[ic]])))
  }

  # create a long data-frame with the reported values for both pathogens
  data_df_list = list()
  plot_dates = c(dates_both_data, dates_fore)
  
  for (ip in 1:npath) {
    data_df_list[[ip]] = data.frame(
      # date = as.Date(dates_both, format = '%Y-%m-%d'),
      date = plot_dates,
      disease = rep(disease_list[[ip]], length(plot_dates)),
      reported = c(obs_each_list[[ip]], rep(NA, npad))
      )
  }
  data_df = rbind(data_df_list[[1]], data_df_list[[2]])
  
  for (ic in 1:length(combined_names)) {

    apply(simdat_both[[ic]],2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles_both

    quantiles_both <- t(quantiles_both)
    quantiles_both <- as.data.frame(quantiles_both)
    quantiles_pad = as.data.frame(matrix(data=NA, nrow=npad_fit, 
                                         ncol=ncol(quantiles_both))
                                 )
    names(quantiles_pad) = names(quantiles_both)
    quantiles_both = rbind(quantiles_pad, quantiles_both)

    forecast_traj[[combined_names[[ic]]]] = list(traj = simdat_both[[ic]],
                                                 date = plot_dates,
                                                 reported_both = reported_both,
                                                 reported_fit_both = reported_fit_both)

    total_both=cbind(date = plot_dates,
                     quantiles_both,
                     reported = c(obs_both, rep(NA, npad)),
                     reported_fit = c(obs_fit_both, rep(NA, npad)))

    total_list[[combined_names[[ic]]]] = total_both

    # copy_total_both = total_both
    # total_both[1:length(obs_fit_both), c('2.5%', '25%', '50%', '75%', '97.5%')] <- NA
    
    if (combined_names[ic]=="err_cor") {
      mytitle = paste0(reg_name,' - Combined Burden (err_cor=', err_cor, ')')
    } else {
      mytitle = paste0(reg_name,' - Combined Burden (err_cor=0.0)')
    }
    

    # y-label only on left most plot
    if (ic == 1) {
      ylab = paste0(cadence_lab, ' New Hosp')
    } else {
      ylab=""
    }

    xlab = paste0(start_year,' - ', end_year)
    mycolor = mycolor_list_with_transparency[['combined']]

    vertical_line <- data.frame(
      x = dates[ntimes],  # Specify the x-coordinate where the vertical line should be
      y = c(0, both_max)  # Specify the y-coordinate range (adjust as needed)
    )

    pl[[combined_names[ic]]] <- ggplot(data=total_both,
                                       mapping=aes(x=date))+
      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='blue',alpha=0.5) +
      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill='blue',alpha=0.8) +
      geom_line(aes(y=`50%`),color='black') +
      geom_col(data = data_df, aes(x = date, y=reported, fill = disease), alpha = 0.5, inherit.aes = FALSE) +
      coord_cartesian(ylim=c(0, both_max)) +
      labs(y=ylab,x=xlab) +
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.7), legend.position = "none") +
      annotate("text", x = median(total_both$date), y = 0.93*both_max, label = mytitle, size = 4)


  }

  
  interactive_plot <- list()

  for (ip in 1:npath) {
    interactive_plot[[ip]] <- ggplotly(pl[[ip]])
  }

  cat("\nMaking Plots\n\n")
  
  # suppressWarnings(print(grid.arrange(ggplotly(pl[[1]]), ggplotly(pl[[2]]), ggplotly(pl[[3]]), ggplotly(pl[[4]]), ncol = 2)))
  suppressWarnings(print(grid.arrange(pl[[1]], pl[[2]], pl[[3]], pl[[4]], ncol = 2)))

  if (!is.null(filename)) {
      suppressWarnings(grid_plots <- grid.arrange(pl[[1]], pl[[2]], pl[[3]], pl[[4]], ncol = 2))
      ggsave(filename = filename, plot = grid_plots, width = 14, height = 6, dpi = 300)
      cat("\n Saving Forecast Plots to: ", filename,'\n')
  }

  # return forecast_traj

  return(list(forecast_traj = forecast_traj, total_list = total_list, wis_df = long_df))
}
