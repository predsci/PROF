#'
#' @title  Baseline statistical forecast
#' @description Calculates and plots a baseline statistical forecast for to the dataset(s)
#'
#' @param prof_data data structure for diseases
#' for each disease it includes:
#' disease - covid19 or influenza
#' population - population size
#' loc_name - location name
#' inc_type - incidence type, e.g. hosp_admits
#' data - all available data as a 2D structure of dates and incidence
#' data_fit - subset of data for fitting (can be eqaul to data)
#' @param ntraj - integer number of stochastic trajectories, default 1000
#' @param nfrcst - number of days to produce a forecast for (MUST be in days), default is 28 days
#' @param filename - if NULL print plot to screen, if not save also to filename
#' @param err_cor numeric scalar [-1, 1] - specify the error correlation to be used
#'  for combining influenza and COVID19 forecasts.  If NULL, default behavior is
#'  to evaluate for err_cor=1 (ordered) and err_cor=0 (random).
#' @param method_name character - method used to aggregate pathogen forecasts
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
plot_stat_forecast <- function(prof_data, ntraj = NULL, nfrcst = NULL,
                               filename = NULL, err_cor = 1.0,
                               method_name="semi_sorted_randA") {

  if (is.null(ntraj)) ntraj = 1000
  if (is.null(nfrcst)) nfrcst = 28

  diseases = names(prof_data)

  npath = length(diseases)

  disease_list = diseases

  pl = simdat_list = dates_frcst_list = total_list = list()

  forecast_traj = list()

  reported_list = reported_fit_list = list()

  wis_df = list()

  # https://www.statology.org/ggplot-default-colors/#:~:text=By%20default%2C%20ggplot2%20chooses%20to,and%20blue%20for%20the%20bars.&text=Here's%20how%20to%20interpret%20the,in%20the%20plot%20is%20%2300BA38.

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

  # loop on all diseases
  for (ip in 1:npath) {


    disease = disease_list[ip]

    mydata = prof_data[[disease]]

    reg_name = mydata$loc_name

    # hospitalization incidence - fitted
    inc = mydata$data_fit_stat$inc

    # dates - fitted
    dates_fit  = mydata$data_fit_stat$date

    ndates = length(dates_fit)

    # using the date array build an integer day array

    times = dates_to_int(dates_fit)

    ntimes = length(times)

    cadence = as.numeric(dates_fit[2]-dates_fit[1])

    # since nfrcst is in days we need to convert it to the units of the observations
    # which can be day or week

    ntimes_frcst= ndates + nfrcst/cadence

    # build also the arrays for the forecasts

    if (cadence == 1) {
      cadence_lab = paste0(cadence, ' day')
      print_lab = 'Days'
      dates_frcst = seq(from = dates_fit[1], length = ntimes_frcst, by = '1 day')
    }

    if (cadence == 7) {
      cadence_lab = paste0(cadence, ' week')
      print_lab = 'Weeks'
      dates_frcst = seq(from = dates_fit[1], length = ntimes_frcst, by = '1 week')
      if (nfrcst %% 7 != 0) {
        print("\nError: For weekly data nfrcst must be a prodcut of seven (e.g., 7, 14, 21 ..) \n")
        return()
      }
    }

    times_frcst = dates_to_int(dates_frcst)

    dates_frcst_list[[ip]] = dates_frcst

    # observations - all data stream

    obs = mydata$data$inc

    # obs may not have the same start date as obs_fit hence need to trim
    keep_ind = which(mydata$data$date >= dates_fit[1])

    obs = obs[keep_ind]
    dates = mydata$data$date[keep_ind]

    obs_fit = mydata$data_fit_stat$inc

    cat("\nCreating ", toupper(disease), " Statistical Forecast for ",reg_name," ",
        print_lab, " Forward for \n\n")


    simdat = stat_forecast(mydata, ntraj, nfrcst/cadence)

    simdat_list[[ip]] = simdat

    # we can score the forecast if the data overlaps the forecast
    #

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

      wis_df[[disease]] = data.frame(date=dates_score, wis = wis_arr, disease = disease, model = 'stat')

    }

    npad = nfrcst/cadence - length(obs)
    if (npad > 0) {
      reported = c(obs, rep(NA, npad))
    } else {
      reported = obs[1:length(dates_frcst)]
    }

    npad_fit = nfrcst/cadence - length(obs_fit)

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
    total=cbind(date = as.Date(dates_frcst, format = '%Y-%m-%d'),time = times_frcst,quantiles,
                reported = reported, reported_fit = reported_fit)

    # Remove 'X' from column names
    #colnames(total) <- gsub("X", "", colnames(total))

    total = as.data.frame(total)

    total_list[[disease]] = total

    total = as.data.frame(total)

    reported_list[[disease]] = reported
    reported_fit_list[[disease]] = reported_fit

    copy_total = total

    total[1:length(obs_fit), c('2.5%','25%','50%','75%','97.5%')] <- NA

    if (cadence == 1) cadence_lab = 'Daily'
    if (cadence == 7) cadence_lab = 'Weekly'

    # y-label only on left most plot
    if (ip == 1) {
      ylab = paste0(cadence_lab, ' New Hosp')
      #xlab = paste0(start_year,' - ', end_year)
    } else {
      ylab = ''
    }

    mycolor = mycolor_list_with_transparency[[disease]]

    mytitle = paste0(reg_name,' - ', toupper(disease), ' Statistical Baseline Model')

    start_year = lubridate::year(range(dates)[1])
    end_year   = lubridate::year(range(dates)[2])
    xlab = paste0(start_year,' - ', end_year)
    if (npath == 2) xlab = ' '

    vertical_line <- data.frame(
      x = dates[ntimes],  # Specify the x-coordinate where the vertical line should be
      y = c(0, max(total[,"97.5%"]))  # Specify the y-coordinate range (adjust as needed)
    )

    pl[[disease]] <- ggplot(data=total,aes(x=date))+
      geom_col(aes(y=reported), fill = mycolor, alpha = 1.) +
      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='blue',alpha=0.5)+
      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill='blue',alpha=0.8)+
      geom_line(aes(y=`50%`),color='black')+
      labs(y=ylab,x=xlab) +
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.7), legend.position = "none") +
      annotate("text", x = median(total$date), y = 0.93*max(total[,c('reported',"97.5%")], na.rm=TRUE), label = mytitle, size = 4)

  } #end of loop over diseases


  if (length(wis_df) !=0) {
    long_df = bind_rows(wis_df)
  } else {
    long_df = NULL
  }

  if (npath == 1) {

    arrange_plot <- ggplotly(pl[[1]])

    if (!is.null(filename)) {
      ggsave(filename = filename, plot = last_plot(), width = 7, height = 6, dpi = 300)
      cat("\n Saving Forecast Plots to: ", filename,'\n')
    }
    return(list(arrange_plot = arrange_plot,total_list = total_list, wis_df = long_df))
  }

  # Combine forecasts
  cat("Combining Forecasts \n")

  combined_frcst_ecor <- combine_fore_err_corr(prof_data, dates_frcst_list, simdat_list,
                                               err_corr=err_cor, nfrcst=nfrcst/cadence,
                                               method_name=method_name)
  combined_frcst_rand <- combine_fore_err_corr(prof_data,
                                               dates_frcst_list, simdat_list,
                                               err_corr=0, nfrcst=nfrcst/cadence,
                                               method_name=method_name)

  combined_frcst = combined_frcst_ecor
  combined_frcst$simdat_both[[2]] = combined_frcst_rand$simdat_both[[1]]

  combined_names <- c("err_cor", "random")
  names(combined_frcst$simdat_both) = combined_names

  # combined_frcst <- combine_forecasts(prof_data, dates_frcst_list, simdat_list)

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

    # apply(simdat_both[[ic]],2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles_both
    #
    # quantiles_both <- t(quantiles_both)
    # quantiles_both <- as.data.frame(quantiles_both)
    #
    # forecast_traj[[combined_names[[ic]]]] = list(traj = simdat_both[[ic]],
    #                                              date = as.Date(dates_both, format = '%Y-%m-%d'),
    #                                              reported_both = reported_both,
    #                                              reported_fit_both = reported_fit_both)
    #
    # total_both=cbind(date = as.Date(dates_both, format = '%Y-%m-%d'),quantiles_both,
    #                  reported = c(obs_both, rep(NA, length(dates_both)-length(obs_both))),
    #                  reported_fit = c(obs_fit_both, rep(NA, length(dates_both)-length(obs_fit_both))))
    #
    # total_list[[combined_names[ic]]] = total_both
    #
    # copy_total_both = total_both
    # total_both[1:length(obs_fit_both), c('2.5%', '25%', '50%', '75%', '97.5%')] <- NA

    if (combined_names[ic]=="err_cor") {
      mytitle = paste0(reg_name,' - Combined Burden (err_cor=', err_cor, ')')
    } else {
      mytitle = paste0(reg_name,' - Combined Burden (err_cor=0)')
    }

    # y-label only on left most plot
    if (ic == 1) {
      ylab = paste0(cadence_lab, ' New Hosp')
    } else {
      ylab=""
    }

    xlab = paste0(start_year,' - ', end_year)
    mycolor =mycolor_list_with_transparency[['combined']]

    vertical_line <- data.frame(
      x = dates[ntimes],  # Specify the x-coordinate where the vertical line should be
      y = c(0, both_max)  # Specify the y-coordinate range (adjust as needed)
    )


    pl[[combined_names[ic]]] <- ggplot(data=data_df,
                                       mapping=aes(x=date))+
      geom_col(aes(y=reported, fill = disease, alpha = 0.5), alpha = 0.5) +
      geom_ribbon(data=total_both, aes(x=date, ymin=`2.5%`,ymax=`97.5%`),fill='blue',alpha=0.5, inherit.aes = FALSE) +
      geom_ribbon(data=total_both, aes(x=date, ymin=`25%`,ymax=`75%`),fill='blue',alpha=0.8, inherit.aes = FALSE) +
      geom_line(data=total_both, aes(x= date, y=`50%`),color='black',  inherit.aes = FALSE) +
      coord_cartesian(ylim=c(0, both_max)) +
      labs(y=ylab,x=xlab) +
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.7), legend.position = "none") +
      annotate("text", x = median(total_both$date), y = 0.93*both_max, label = mytitle, size = 4)


  }

  interactive_plot <- list()

  for (ip in 1:length(pl)) {
    interactive_plot[[ip]] <- ggplotly(pl[[ip]])
  }

  cat("\nMaking Plots\n\n")

  arrange_plot <- subplot(interactive_plot[[1]], interactive_plot[[2]],
                          interactive_plot[[3]], interactive_plot[[4]],
                          nrows = 2, titleX = TRUE, titleY = TRUE)



  if (!is.null(filename)) {
    suppressWarnings(grid_plots <- grid.arrange(pl[[1]], pl[[2]], pl[[3]], pl[[4]], ncol = 2))
    ggsave(filename = filename, plot = grid_plots, width = 14, height = 6, dpi = 300)
    cat("\n Saving Forecast Plots to: ", filename,'\n')
  }

  # return a list

  return(list(total_list = total_list, wis_df = long_df, arrange_plot = arrange_plot))
}
