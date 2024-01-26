#'
#' Plots the Results of a Baseline Statistical Model to the dataset(s)
#'
#'
#' @param prof_data data structure for diseases
#' for each disease it includes:
#' disease - covid19 or influenza
#' population - population size
#' loc_name - location name
#' inc_type - incidence type, e.g. hosp_admits
#' data - all available data as a 2D structure of dates and incidence
#' data_fit - subset of data for fitting (can be eqaul to data)
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
plot_stat_forecast <- function(prof_data, ntraj = NULL, nfrcst = NULL, filename = NULL) {

  if (is.null(ntraj)) ntraj = 1000
  if (is.null(nfrcst)) nfrcst = 35

  npath = length(prof_data)

  disease_list = names(prof_data)

  pl = simdat_list = dates_frcst_list = list()

  forecast_traj = list()

  # loop on all diseases
  for (ip in 1:npath) {

    disease = disease_list[ip]

    mydata = prof_data[[disease]]

    reg_name = mydata$loc_name

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

    # observations - all data stream

    obs = mydata$data$inc

    # obs may not have the sam start date as obs_fit hence need to trim
    keep_ind = which(mydata$data$date >= dates_fit[1])

    obs = obs[keep_ind]
    dates = mydata$data$date[keep_ind]

    obs_fit = mydata$data_fit$inc

    cat('\nCreating ',toupper(disease),' Statistical Forecast for ', reg_name,' ', nfrcst,' Days Forward\n\n')
    simdat = stat_forecast(mydata, ntraj, nfrcst)

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

    title = paste0(reg_name,' - ', toupper(disease),' Statistical Baseline Model')

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
  if (npath > 1) cat("Combining Forecasts \n")

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

  # find maximum in simdat_both of random and sorted and use

  both_max = 0.0
  for (ip in 1:npath) {
    both_max = max(both_max, round(max(simdat_both[[ip]])))
  }

  for (ip in 1:npath) {

  apply(simdat_both[[ip]],2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles_both

  quantiles_both <- t(quantiles_both)
  quantiles_both <- as.data.frame(quantiles_both)

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
