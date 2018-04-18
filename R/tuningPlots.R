#' @title Create a tuning line plot
#'
#' @description
#' This plot is just usable if the algorithm.parameter column contains an iteration value.
#' Create a plotly line plot out of a benchmarkVis compatible data table.
#' The created line plot shows the change of the specified measure for each iteration.
#' A cumulative function can be defined to get a better feeling of the course of the line chart.
#' Possible parameters for the cumulative function are: "id" (identity function), "min", "max", "mean" - default is min.
#' If desired a histogram of the measure values can be plotted in the background to easily see the distribution in the data.table.
#' Additionally a algorithm parameter can be defined to be plotted as circles on top of the line chart.
#' This way the connection between measure result and parameter can be investigated.
#' x-Axis: the iteration.
#' y-Axis: the measure.
#' y2-Axis (on the right): the optional algorithm parameter

#' @param dt compatible data table
#' @param measure the column name containing the results of a measure
#' @param cumulative.function the cumulative function to use for the measure values (default: "min")
#' @param show.histogram shows the histogram of the measure values in the background (default: FALSE)
#' @param tuning.parameter the algorithm parameter (default: "none")
#' @return a plotly line plot
#' @export
#' @examples
#' createTuningLinePlot(mlr.tuning.example, "measure.acc.test.mean", "id", TRUE, "C")
createTuningLinePlot = function(dt,
  measure,
  cumulative.function = "min",
  show.histogram = FALSE,
  tuning.parameter = "none") {
  # Checks
  checkmate::assert_true(getTuningsCount(dt) > 0)
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_true(measure %in% getMeasures(dt))
  checkmate::assert_string(cumulative.function)
  checkmate::assert_true(cumulative.function %in% c("id", "max", "min", "mean"))
  checkmate::assert_logical(show.histogram)
  checkmate::assert_string(tuning.parameter)
  # Create new data frame as filtered dt
  new.df = data.frame()
  for (entry in getTunings(dt)) {
    if ("replication" %in% getMainColumns(dt)) {
      filtered = dt[dt$problem == entry[1] & dt$algorithm == entry[2] & dt$replication == entry[3], ]
    } else {
      filtered = dt[dt$problem == entry[1] & dt$algorithm == entry[2], ]
    }
    filtered$measure = getCumulativeValues(filtered[[measure]], cumulative.function)
    new.df = rbind(new.df, filtered)
  }
  checkmate::assert_true(all(sapply(new.df$algorithm.parameter, function(x) {
    tuning.parameter %in% names(x)
  })) || tuning.parameter == "none")
  # Get parameter for new data table
  new.df$iteration = sapply(new.df$algorithm.parameter, function(x) {
    return(x$iteration)
  })
  new.df$param.text = sapply(new.df$algorithm.parameter, function(x) {
    # Get index of iteration in algorithm.paramter
    id = which(names(x) %in% "iteration")
    return(toString(paste(names(x)[-id], x[-id], sep = " = ")))
  })
  # define color
  if ("replication" %in% getMainColumns(dt)) {
    new.df$color = interaction(new.df$problem, new.df$algorithm, new.df$replicaiton)
  } else {
    new.df$color = interaction(new.df$problem, new.df$algorithm)
  }
  # Create plot
  p = plotly::plot_ly(new.df)
  p = plotly::add_trace(
    p,
    x = ~ iteration,
    y = ~ measure,
    name = getPrettyMeasureName(measure),
    text = ~ param.text,
    type = "scatter",
    mode = "lines+markers",
    color = ~ color
  )
  # Plot optional histogram
  if (show.histogram) {
    p = plotly::add_histogram(p,
      y = ~ measure,
      alpha = 0.4,
      name = "Measure distribution")
  }
  # Plot optional parameter scatters
  if (tuning.parameter != "none") {
    # Add parameter to new.df if parameter is defined
    parameter.values = sapply(new.df$algorithm.parameter, function(x) {
      return(x[[tuning.parameter]])
    })
    p = plotly::add_trace(
      p,
      x = ~ iteration,
      y = parameter.values,
      name = tuning.parameter,
      type = "scatter",
      mode = "markers",
      color = ~ color,
      yaxis = "y2"
    )
    p = plotly::layout(
      p,
      xaxis = list(title = "iteration"),
      yaxis = list(overlaying = "y2", title = getPrettyMeasureName(measure)),
      yaxis2 = list(side = "right", title = tuning.parameter)
    )
  } else {
    p = plotly::layout(
      p,
      xaxis = list(title = "iteration"),
      yaxis = list(title = getPrettyMeasureName(measure))
    )
  }
  return(p)
}
