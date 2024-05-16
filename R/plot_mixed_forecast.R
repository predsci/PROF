#'
#' @title Create a Mixed Forecast
#' @description Use the mechanistic and statistical forecasts to create a mixed forecast
#' Forecast are combined for each pathogen separately
#' @param prof_data data structure for diseases
#' @param forecast_list the list output of plot_forecast function
#' @param forecast_stat_list the list output of plot_stat_forecast function
#' @return
#' arrange_plot - a list of plot(s)
#' total - data frame(s) used for the plots
#' forecast_mixed - a list with the mixed forecast for each pathogen
#' @export
#'
plot_mixed_forecast <- function(prof_data, forecast_list, forecast_stat_list) {

  disease_list= names(forecast_list$forecast_traj)

  npath = length(diseases)

  pl = forecast_mixed = total_list = list()

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

  # loop on all diseases in forecast lists

  for (ip in 1:npath) {

    disease = disease_list[ip]

    mydata = prof_data[[disease]]

    reg_name = mydata$loc_name

    forecast_traj = forecast_list$forecast_traj[[disease]]$traj

    forecast_stat_traj = forecast_stat_list$forecast_traj[[disease]]$traj

    cat("\nCreating Mixed Forecase for: ", toupper(disease), ' ', reg_name,'\n')
    # create a mixed forecast

    dates_forecast = forecast_list$forecast_traj[[disease]]$date

    times_frcst = dates_to_int(dates_forecast)

    ntimes_frcst = max(times_frcst)

    cadence = as.numeric(dates_forecast[2]-dates_forecast[1])

    reported = forecast_list$forecast_traj[[disease]]$reported

    reported_fit = forecast_list$forecast_traj[[disease]]$reported_fit

    forecast_both = rbind(forecast_traj, forecast_stat_traj)

    ntraj = dim(forecast_both)[1]

    #
    last_non_na_index <- max(which(!is.na(reported_fit)))
    if (cadence == 7) {
      obs_mean = reported_fit[last_non_na_index]
      obs_model = mean(forecast_both[,last_non_na_index])
    } else {
      obs_mean = mean(reported_fit[(last_non_na_index-7+1):last_non_na_index])
      obs_model = mean(forecast_both[,(last_non_na_index-7+1):last_non_na_index])
    }

    shift = obs_mean - obs_model

    for (ii in 1:ntraj) forecast_both[ii, ] = pmax(forecast_both[ii, ] + shift,0)

    forecast_mixed[[disease]] = forecast_both

    apply(forecast_both,2,quantile,probs=c(0.025,0.25,0.5,0.75,0.975)) -> quantiles
    quantiles <- t(quantiles)
    quantiles <- as.data.frame(quantiles)
    total=cbind(date = as.Date(dates_forecast, format = '%Y-%m-%d'),time = times_frcst,quantiles,
                reported = reported, reported_fit = reported_fit)

    total = as.data.frame(total)

    copy_total = total

    total[!is.na(reported_fit), c('2.5%','25%','50%','75%','97.5%')] <- NA

    total_list[[disease]] = total

    if (cadence == 1) cadence_lab = 'Daily'
    if (cadence == 7) cadence_lab = 'Weekly'

    ylab = paste0(cadence_lab, ' New Hosp')

    disease = 'influenza'

    mycolor = mycolor_list_with_transparency[[disease]]

    mytitle = paste0(reg_name,' - ', toupper(disease), ' Mixed Model')

    start_year = lubridate::year(range(dates_forecast)[1])
    end_year   = lubridate::year(range(dates_forecast)[2])
    xlab = paste0(start_year,' - ', end_year)

    pl[[disease]] <- ggplot(data=total,aes(x=date))+
      geom_col(aes(y=reported), fill = mycolor, alpha = 1.) +
      geom_ribbon(aes(ymin=`2.5%`,ymax=`97.5%`),fill='blue',alpha=0.5)+
      geom_ribbon(aes(ymin=`25%`,ymax=`75%`),fill='blue',alpha=0.8)+
      geom_line(aes(y=`50%`),color='black')+
      labs(y=ylab,x=xlab) +
      theme(plot.title = element_text(hjust = 0.5, vjust = 0.7), legend.position = "none") +
      annotate("text", x = median(total$date), y = 0.93*max(total[,c('reported',"97.5%")], na.rm=TRUE), label = mytitle, size = 4)
  } # end of loop over diseases

  if (npath > 1) {
    interactive_plot = list()
    for (ip in 1:npath) {
      interactive_plot[[ip]] <- ggplotly(pl[[ip]])
    }
    arrange_plot <- subplot(interactive_plot[[1]], interactive_plot[[2]],
                            nrows = 1, titleX = TRUE, titleY = TRUE)
  } else {
    arrange_plot <- ggplotly(pl[[1]])
  }
  return(list(arrange_plot = arrange_plot, total = total_list, forecast_mixed = forecast_both))


}
