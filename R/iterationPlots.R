#' @title Create a iteration parameter scatter plot
#'
#' @description
#' This plot is just usable if the algorithm.parameter column contains an iteration value.
#' Create a plotly scatter to plot out of a benchmarkVis compatible data table.
#' The created scatter plot shows the connection between the specified measure and algorithm parameter.
#' x-Axis: the algorithm parameter.
#' y-Axis: the measure.
#'
#' @param dt compatible data table
#' @param measure the column name containing the results of a measure
#' @param parameter the algorithm parameter you want to examine
#' @return a plotly scatter plot
#' @export
#' @examples
#' createIterationParameterPlot(mlr.tuning.example, "measure.acc.test.mean", "C")
createIterationParameterPlot = function(dt, measure, parameter) {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(parameter)
  checkmate::assert_string(measure)
  checkmate::assert_true(all(sapply(dt$algorithm.parameter, function(x) {
    parameter %in% names(x)
  })))
  checkmate::assert_true(measure %in% getMeasures(dt))
  # Get parameter for new data table
  param = sapply(dt$algorithm.parameter, function(x) {
    return(x[[parameter]])
  })
  # Create new plotly compatible data frame
  new.df = data.frame(parameter = param,
    measure = dt[[measure]])
  # Create plot
  p = plotly::plot_ly(
    new.df,
    x = ~ parameter,
    y = ~ measure,
    type = "scatter",
    mode = "markers"
  )
  p = plotly::layout(p,
    yaxis = list(title = measure),
    xaxis = list(title = parameter))
  return(p)
}

#' @title Create a iteration dual parameter scatter plot
#'
#' @description
#' This plot is just usable if the algorithm.parameter column contains an iteration value.
#' Create a plotly scatter to plot out of a benchmarkVis compatible data table.
#' The created scatter plot shows the connection between the specified measure and two different algorithm parameters.
#' x-Axis: the second parameter - default is iteration.
#' y-Axis: the first parameter.
#' Size and color of the circles: Depends on the measure value.
#'
#' @param dt compatible data table
#' @param measure the column name containing the results of a measure
#' @param parameter the first parameter you want to examine
#' @param parameter2 the second parameter you want to examine (default: "iteration")
#' @return a plotly scatter plot
#' @export
#' @examples
#' createIterationDualParameterPlot(mlr.tuning.example, "measure.acc.test.mean", "C", "sigma")
createIterationDualParameterPlot = function(dt, measure, parameter, parameter2 = "iteration") {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(parameter)
  checkmate::assert_string(parameter2)
  checkmate::assert_string(measure)
  checkmate::assert_true(all(sapply(dt$algorithm.parameter, function(x) {
    parameter %in% names(x)
  })))
  checkmate::assert_true(all(sapply(dt$algorithm.parameter, function(x) {
    parameter2 %in% names(x)
  })))
  checkmate::assert_true(measure %in% getMeasures(dt))
  # Get parameter for new data table
  param = sapply(dt$algorithm.parameter, function(x) {
    return(x[[parameter]])
  })
  param2 = sapply(dt$algorithm.parameter, function(x) {
    return(x[[parameter2]])
  })
  # Create new plotly compatible data frame
  new.df = data.frame(parameter = param,
    parameter2 = param2,
    measure = dt[[measure]])
  # Create plot
  p = plotly::plot_ly(
    new.df,
    x = ~ parameter2,
    y = ~ parameter,
    type = "scatter",
    mode = "markers",
    color = ~ measure,
    size = ~ measure,
    colors = c("green", "blue"),
    marker = list(colorbar = list(title = measure))
  )
  p = plotly::layout(p,
    yaxis = list(title = parameter),
    xaxis = list(title = parameter2))
  return(p)
}

#' @title Create a iteration line plot
#'
#' @description
#' This plot is just usable if the algorithm.parameter column contains an iteration value.
#' Create a plotly line plot out of a benchmarkVis compatible data table.
#' The created line plot shows the change of the specified measure for each iteration
#' A cumulative function can be defined to get a better feeling of the course of the line chart.
#' Possible parameters for the cumulative function are: "id" (identity function), "min", "max", "mean" - default is min.
#' If desired a histogram of the measure values can be plotted in the background to easily see the distribution in the data.table.
#' Additionally a algorithm parameter can be defined to be plotted as circles on top of the line chart.
#' Thereby the connection between measure and parameter can be investigated.
#' x-Axis: the iteration.
#' y-Axis: the measure.
#' y2-Axis (on the right): the optional algorithm parameter

#' @param dt compatible data table
#' @param measure the column name containing the results of a measure
#' @param cumulative.funtion the cumulative function to use for the measure values (default: "min")
#' @param show.histogram shows the histogram of the measure values in the background (default: TRZUE)
#' @param parameter the algorithm parameter
#' @return a plotly line plot
#' @export
#' @examples
#' createIterationPlot(mlr.tuning.example, "measure.acc.test.mean", "id", TRUE, "C")
createIterationPlot = function(dt,
  measure,
  cumulative.function = "min",
  show.histogram = TRUE,
  parameter = "None") {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_string(parameter)
  checkmate::assert_true(all(sapply(dt$algorithm.parameter, function(x) {
    parameter %in% names(x)
  })) || parameter == "None")
  checkmate::assert_true(measure %in% getMeasures(dt))
  # Get parameter for new data table
  iter = sapply(dt$algorithm.parameter, function(x) {
    return(x$iteration)
  })
  param.text = sapply(dt$algorithm.parameter, function(x) {
    # Get index of iteration in algorithm.paramter
    id = which(names(x) %in% "iteration")
    return(toString(paste(names(x)[-id], x[-id], sep = " = ")))
  })
  meas = getCumulativeValues(dt[[measure]], cumulative.function)
  # Create new plotly compatible data frame
  new.df = data.frame(iteration = iter,
    measure = meas,
    parameter.text = param.text)
  # Add parameter to new.df if parameter is defined
  if (parameter != "None") {
    param = sapply(dt$algorithm.parameter, function(x) {
      return(x[[parameter]])
    })
    new.df$parameter = param
  }
  # Create plot
  p = plotly::plot_ly(new.df)
  p = plotly::add_trace(
    p,
    x = ~ iteration,
    y = ~ measure,
    name = measure,
    text = ~ parameter.text,
    type = "scatter",
    mode = "lines+markers"
  )
  if (show.histogram) {
    p = plotly::add_histogram(p,
      y = ~ measure,
      alpha = 0.4,
      name = "Measure distribution")
  }
  if (parameter != "None") {
    p = plotly::add_trace(
      p,
      x = ~ iteration,
      y = ~ parameter,
      name = parameter,
      type = "scatter",
      mode = "markers",
      yaxis = "y2"
    )
    p = plotly::layout(
      p,
      xaxis = list(title = "iteration"),
      yaxis = list(overlaying = "y2", title = measure),
      yaxis2 = list(side = "right", title = parameter)
    )
  } else {
    p = plotly::layout(p,
      xaxis = list(title = "iteration"),
      yaxis = list(title = measure))
  }
  return(p)
}
