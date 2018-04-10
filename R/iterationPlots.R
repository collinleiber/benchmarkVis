#' @title Create a iteration parameter scatter plot
#'
#' @description
#' This plot is just usable if the algorithm.parameter column contains an iteration value.
#' Create a plotly scatter plot out of a benchmarkVis compatible data table.
#' The created scatter plot shows the connection between the specified measure and algorithm parameter.
#' x-Axis: the algorithm parameter.
#' y-Axis: the measure.
#'
#' @param dt compatible data table
#' @param measure the column name containing the results of a measure
#' @param parameter the algorithm parameter you want to examine
#' @param show.histogram shows the histogram of the measure values in the background (default: FALSE)
#' @param regression.line add a regression line to the plot (default: FALSE)
#' @param iteration.algorithm the algorithm to investigate. Algorithm.parameter must contain "iteration" field.
#' (default: "default" - would take the first from getIterationAlgorithms())
#' @return a plotly scatter plot
#' @export
#' @examples
#' createIterationParameterPlot(mlr.tuning.example, "measure.acc.test.mean", "C", TRUE)
createIterationParameterPlot = function(dt,
  measure,
  parameter,
  show.histogram = FALSE,
  regression.line = FALSE,
  iteration.algorithm = "default") {
  # Get first iteration algorithm
  if (iteration.algorithm == "default") {
    iteration.algorithm = getIterationAlgorithms(dt)[1]
  }
  dt = filterTableForIterationAlgorithm(dt, iteration.algorithm)
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(parameter)
  checkmate::assert_string(measure)
  checkmate::assert_logical(show.histogram)
  checkmate::assert_logical(regression.line)
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
    new.df
  )
  p = plotly::add_trace(p, x = ~ parameter,
    y = ~ measure,
    name = "Results",
    type = "scatter",
    mode = "markers")
  # Add regression line
  if (regression.line) {
    fit = lm(measure ~ parameter, data = new.df)
    p = plotly::add_lines(p, x = ~parameter, y = fitted(fit), name = "regression")
  }
  # Plot optional histogram
  if (show.histogram) {
    p = plotly::add_histogram(p,
      y = ~ measure,
      alpha = 0.4,
      name = "Measure distribution",
      xaxis = "x2")
    p = plotly::layout(
      p,
      title = iteration.algorithm,
      yaxis = list(title = measure),
      xaxis = list(title = parameter, overlaying = "x2"),
      xaxis2 = list(side = "top", title = "Count"),
      margin = list(t = 130)
    )
  } else {
    p = plotly::layout(
      p,
      title = iteration.algorithm,
      yaxis = list(title = measure),
      xaxis = list(title = parameter)
    )
  }
  return(p)
}

#' @title Create a iteration dual parameter scatter plot
#'
#' @description
#' This plot is just usable if the algorithm.parameter column contains an iteration value.
#' Create a plotly scatter plot out of a benchmarkVis compatible data table.
#' The created scatter plot shows the connection between the specified measure and two different algorithm parameters.
#' x-Axis: the second parameter - default is iteration.
#' y-Axis: the first parameter.
#' Size and color of the circles: Depends on the measure value.
#'
#' @param dt compatible data table
#' @param measure the column name containing the results of a measure
#' @param parameter the first parameter you want to examine
#' @param parameter2 the second parameter you want to examine (default: "iteration")
#' @param plot.area plot the measure as an area instead of scatters (default: FALSE)
#' @param iteration.algorithm the algorithm to investigate. Algorithm.parameter must contain "iteration" field.
#' (default: "default" - would take the first from getIterationAlgorithms())
#' @return a plotly scatter plot
#' @export
#' @examples
#' createIterationDualParameterPlot(mlr.tuning.example, "measure.acc.test.mean", "C", "sigma")
createIterationDualParameterPlot = function(dt,
  measure,
  parameter,
  parameter2 = "iteration",
  plot.area = FALSE,
  iteration.algorithm = "default") {
  # Get first iteration algorithm
  if (iteration.algorithm == "default") {
    iteration.algorithm = getIterationAlgorithms(dt)[1]
  }
  dt = filterTableForIterationAlgorithm(dt, iteration.algorithm)
  # Checks
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
  new.df = data.frame(param = param,
    param2 = param2,
    me = dt[[measure]])
  # Create plot
  if (!plot.area) {
    p = plotly::plot_ly(
      new.df,
      x = ~ param2,
      y = ~ param,
      type = "scatter",
      mode = "markers",
      color = ~ me,
      size = ~ me,
      text = ~ paste(
        parameter2,
        ": ",
        param2,
        "<br>",
        parameter,
        ": ",
        param,
        "<br>",
        measure,
        ": ",
        me,
        sep = ""
      ),
      colors = c("green", "blue"),
      marker = list(colorbar = list(title = measure))
    )
  } else {
    p = plotly::plot_ly(
      new.df,
      x = ~ param2,
      y = ~ param,
      z = ~ me,
      type = "contour"
    )
    p = plotly::colorbar(p, title = measure)
  }
  p = plotly::layout(
    p,
    title = iteration.algorithm,
    yaxis = list(title = parameter),
    xaxis = list(title = parameter2)
  )
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
#' @param cumulative.function the cumulative function to use for the measure values (default: "min")
#' @param show.histogram shows the histogram of the measure values in the background (default: FALSE)
#' @param iteration.algorithm the algorithm to investigate. Algorithm.parameter must contain "iteration" field.
#' (default: "default" - would take the first from getIterationAlgorithms())
#' @param parameter the algorithm parameter
#' @return a plotly line plot
#' @export
#' @examples
#' createIterationLinePlot(mlr.tuning.example, "measure.acc.test.mean", "id", TRUE, "classif.ksvm", "C")
createIterationLinePlot = function(dt,
  measure,
  cumulative.function = "min",
  show.histogram = FALSE,
  iteration.algorithm = "default",
  parameter = "None") {
  # Get first iteration algorithm
  if (iteration.algorithm == "default") {
    iteration.algorithm = getIterationAlgorithms(dt)[1]
  }
  dt = filterTableForIterationAlgorithm(dt, iteration.algorithm)
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_string(parameter)
  checkmate::assert_string(cumulative.function)
  checkmate::assert_logical(show.histogram)
  checkmate::assert_true(cumulative.function %in% c("id", "max", "min", "mean"))
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
  # Plot optional histogram
  if (show.histogram) {
    p = plotly::add_histogram(p,
      y = ~ measure,
      alpha = 0.4,
      name = "Measure distribution")
  }
  # Plot optional parameter scatters
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
      title = iteration.algorithm,
      xaxis = list(title = "iteration"),
      yaxis = list(overlaying = "y2", title = measure),
      yaxis2 = list(side = "right", title = parameter)
    )
  } else {
    p = plotly::layout(
      p,
      title = iteration.algorithm,
      xaxis = list(title = "iteration"),
      yaxis = list(title = measure)
    )
  }
  return(p)
}

#' @title Create a iteration line plot with a measure on x and y aixs
#'
#' @description
#' Create a plotly line plot out of a benchmarkVis compatible data table.
#' The created line chart shows the change within the specified list measures.
#'
#' @param dt campatible data table
#' @param measure1 the measure on the x axis
#' @param measure2 the measure on the y axis
#' @param draw.lines draw a line between the points in the original order (default: FALSE)
#' @param regression.line add a regression line to the plot (default: FALSE)
#' @param iteration.algorithm the algorithm to investigate. Algorithm.parameter must contain "iteration" field.
#' (default: "default" - would take the first from getIterationAlgorithms())
#' @return a plotly line plot
#' @export
#' @examples
#' createIterationDualMeasurePlot(mlr.tuning.example, "measure.acc.test.mean", "measure.acc.test.sd")
createIterationDualMeasurePlot = function(dt, measure1, measure2, draw.lines = FALSE, regression.line = FALSE, iteration.algorithm = "default") {
  # Get first iteration algorithm
  if (iteration.algorithm == "default") {
    iteration.algorithm = getIterationAlgorithms(dt)[1]
  }
  dt = filterTableForIterationAlgorithm(dt, iteration.algorithm)
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_logical(regression.line)
  checkmate::assert_string(measure1)
  checkmate::assert_string(measure2)
  checkmate::assert_true(measure1 %in% getMeasures(dt))
  checkmate::assert_true(measure2 %in% getMeasures(dt))
  # Get iteration
  iter = sapply(dt$algorithm.parameter, function(x) {
    return(x$iteration)
  })
  param.text = sapply(dt$algorithm.parameter, function(x) {
    # Get index of iteration in algorithm.paramter
    return(toString(paste(names(x), x, sep = " = ")))
  })
  # Create new plotly compatible data table
  new.df = data.frame(
    measure1 = dt[[measure1]],
    measure2 = dt[[measure2]],
    problem = dt$problem,
    algorithm = dt$algorithm,
    iteration = iter,
    text = param.text
  )
  # Create plot
  p = plotly::plot_ly(new.df)
  if (draw.lines) {
    p = plotly::add_trace(p, x = ~measure1, y = ~measure2, linetype = ~problem, text = ~text, type = "scatter", mode = "lines+markers")
  } else {
    p = plotly::add_trace(p, x = ~measure1, y = ~measure2, text = ~text, type = "scatter", mode = "markers")
  }
  # Add regression line
  if (regression.line) {
    fit = lm(measure2 ~ measure1, data = new.df)
    p = plotly::add_lines(p, x = ~measure1, y = fitted(fit), name = "regression")
  }
  p = plotly::layout(p, title = iteration.algorithm, xaxis = list(title = measure1), yaxis = list(title = measure2))
  return(p)
}

# Hepler method to get only the data table rows which contain the iteration algorithm.
filterTableForIterationAlgorithm = function(dt, iteration.algorithm) {
  checkmate::assert_string(iteration.algorithm)
  checkmate::assert_true(iteration.algorithm %in% getIterationAlgorithms(dt))
  # Filter data table
  dt = dt[dt$algorithm == iteration.algorithm, ]
  return(dt)
}
