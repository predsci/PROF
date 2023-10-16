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
#'  @filename - if NULL print plot to screen, if not save also to filename
#' @return
#' plots to screen and file and a list of two items:
#' stat_fit_traj - a list with a list for each disease: statistical trajectories,
#' dates, and reported incidence.
#'
#' stat_pl - a list of statistical fit plots
#'
#'
plot_stat_fit <- function(prof_data, ntraj = NULL, filename=NULL) {

  if (is.null(ntraj)) ntraj = 1000

  npath = length(prof_data)

  disease_list = names(prof_data)

  pl = simdat_list =list()

  fit_traj = list()

  # loop on all diseases
  for (ip in 1:npath) {

    disease = disease_list[ip]

    mydata = prof_data[[disease]]

    reg_name = mydata$loc_name

    # hospitalization incidence - fitted
    inc = mydata$data_fit$inc

    # dates - fitted
    dates  = mydata$data_fit$date

    ndates = length(dates)

    # using the date array build an integer day array

    times = dates_to_int(dates)

    ntimes = length(times)

    # observations - all data stream

    obs = mydata$data$inc

    obs_fit = mydata$data_fit$inc

    cat('\nCreating ',toupper(disease),' Statistical Fit for ', reg_name,'\n\n')

    simdat = stat_fit(obs_fit, ntraj)

    simdat_list[[ip]] = simdat

    reported = obs

    reported_fit = obs_fit

    fit_traj[[disease]] = list(traj = simdat,
                                    date = as.Date(dates, format = '%Y-%m-%d'),
                                    reported = reported, reported_fit = reported_fit)

    apply(simdat,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles

    quantiles <- t(quantiles)
    quantiles <- as.data.frame(quantiles)

    total=cbind(date = as.Date(dates, format = '%Y-%m-%d'),time = 1:ntimes,quantiles,
                reported = reported, reported_fit = reported_fit)

    total = as.data.frame(total)

    cadence = as.numeric(dates[2]-dates[1])
    if (cadence == 1) cadence_lab = 'Daily'
    if (cadence == 7) cadence_lab = 'Weekly'

    ylab = paste0(cadence_lab, ' New Hosp')

    title = paste0(reg_name,' - ', toupper(disease),' Statistical Baseline Model')

    start_year = lubridate::year(range(dates)[1])
    end_year   = start_year + 1
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



  cat("\nMaking Plots\n\n")
  if (npath == 2) {

    suppressWarnings(print(grid.arrange(pl[[1]], pl[[2]], ncol = 2)))
    if (!is.null(filename)) {
      suppressWarnings(grid_plots <- grid.arrange(pl[[1]], pl[[2]], ncol = 2))
      ggsave(filename = filename, plot = grid_plots, width = 14, height = 6, dpi = 300)
      cat("\n Saving Forecast Plots to: ", filename,'\n')
    }
  } else {
    suppressWarnings(pl[[1]])
    if (!is.null(filename)) {
      ggsave(filename = filename, plot = last_plot(), width = 7, height = 6, dpi = 300)
      cat("\n Saving Forecast Plots to: ", filename,'\n')
    }

  }

  # return ft_traj
  return(list(stat_fit_traj = fit_traj, pl_stat = pl))

}
