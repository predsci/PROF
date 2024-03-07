#' @title plot_wis
#' @description Plot the WIS score of Forecasts
#' @param wis_data - a list with the WIS score (or NULL) for each forecasted pathogen
#' @param loc string location name
#' @param filename optional for saving plot(s) to a file
#' @return
#' A data frame with all WIS information

plot_wis <- function(wis_data, loc = NA, filename = NULL) {

  if (is.null(wis_data)) {
    print("WIS data is NULL")
    return
  }

  wis_name = names(wis_data)
  nwis = length(wis_name)

  if (nwis == 1){
    wis_df = wis_data[[wis_name[1]]]
  } else {
    wis_df = bind_rows(wis_data)
  }

  pl = list()

  mydiseases= unique(wis_df$disease)

  wis_df$wis = round(wis_df$wis, 2)


  for (jj in 1:length(mydiseases)) {
    mydisease = mydiseases[jj]
    data = subset(wis_df, disease == mydisease)
    title = paste0(toupper(loc), ' WIS SCORE ')
    pl[[jj]] <- ggplot(data, aes(x = date, y = wis, fill = model)) +
      geom_bar(stat = "identity", position = "dodge") +
      annotate("text", x = data$date[2], y = max(data$wis)*0.9, label = toupper(mydisease), size = 5) +
      labs(title = title,
           x = "Date",
           y = "WIS Score",
           fill = "Model")

  }

  interactive_plot <- list()

  # Render the plot

  for (ip in 1:nwis) {
    interactive_plot[[ip]] <- ggplotly(pl[[ip]])
  }

  if (length(pl) == 1) {

    interactive_plot[[1]] <- ggplotly(pl[[1]])

    suppressWarnings(print(interactive_plot[[1]]))

    if (!is.null(filename)) {
      ggsave(filename = filename, plot = last_plot(), width = 7, height = 6, dpi = 300)
      cat("\n Saving Forecast Plots to: ", filename,'\n')
    }
  }


  suppressWarnings(print(grid.arrange(pl[[1]], pl[[2]], ncol = 1)))

  if (!is.null(filename)) {
    suppressWarnings(grid_plots <- grid.arrange(pl[[1]], pl[[2]], ncol = 1))
    ggsave(filename = filename, plot = grid_plots, width = 14, height = 6, dpi = 300)
    cat("\n Saving Forecast Plots to: ", filename,'\n')
  }

  return(wis_df)
}
