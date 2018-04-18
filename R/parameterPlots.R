#' @title Create a parameter scatter plot
#'
#' @description
#' This plot is just usable if at least one parameter column is contained in the data table.
#' Create a plotly scatter plot out of a benchmarkVis compatible data table.
#' The created scatter plot shows the connection between the specified measure and parameter values.
#' x-Axis: the parameter.
#' y-Axis: the measure.
#'
#' @param dt compatible data table
#' @param measure the column name containing the results of a measure
#' @param parameter.column the parameter column which contains the parameter
#' @param parameter the parameter you want to examine
#' @param color.by the column to color the markers with. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @param show.histogram shows the histogram of the measure values in the background (default: FALSE)
#' @param regression.line add a regression line to the plot (default: FALSE)
#' @return a plotly scatter plot
#' @export
#' @examples
#' createParameterScatterPlot(mlr.tuning.example, "measure.acc.test.mean", "algorithm.parameter", "C", "algorithm", TRUE)
createParameterScatterPlot = function(dt,
  measure,
  parameter.column,
  parameter,
  color.by = "algorithm",
  show.histogram = FALSE,
  regression.line = FALSE) {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_true(measure %in% getMeasures(dt))
  checkmate::assert_string(parameter.column)
  checkmate::assert_true(parameter.column %in% getParameterColumns(dt))
  checkmate::assert_string(parameter)
  checkmate::assert_string(color.by)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  checkmate::assert_logical(show.histogram)
  checkmate::assert_logical(regression.line)
  # Create new df as filtered dt
  new.df = dt[sapply(dt[[parameter.column]], function(x) {
    return(parameter %in% names(x))
  }), ]
  checkmate::assert_true(nrow(new.df) > 0)
  # Get parameter for new data table
  new.df$param = sapply(new.df[[parameter.column]], function(x) {
    return(x[[parameter]])
  })
  new.df$me = new.df[[measure]]
  # Create plot
  p = plotly::plot_ly(new.df)
  p = plotly::add_trace(
    p,
    x = ~ param,
    y = ~ me,
    type = "scatter",
    mode = "markers",
    color = new.df[[color.by]],
    text = ~ paste0(
      parameter,
      ": ",
      param,
      "<br>",
      measure,
      ": ",
      me
    )
  )
  # Add regression line
  if (regression.line) {
    fit = lm(me ~ param, data = new.df)
    p = plotly::add_lines(p,
      x = ~ param,
      y = fitted(fit),
      name = "regression")
  }
  # Plot optional histogram
  if (show.histogram) {
    p = plotly::add_histogram(
      p,
      y = ~ me,
      alpha = 0.4,
      name = "Measure distribution",
      xaxis = "x2"
    )
    p = plotly::layout(
      p,
      xaxis = list(title = parameter, overlaying = "x2"),
      yaxis = list(title = getPrettyMeasureName(measure)),
      xaxis2 = list(side = "top", title = "Count"),
      margin = list(t = 130)
    )
  } else {
    p = plotly::layout(
      p,
      xaxis = list(title = parameter),
      yaxis = list(title = getPrettyMeasureName(measure))
    )
  }
  return(p)
}

#' @title Create a parameter dual scatter plot
#'
#' @description
#' This plot is just usable if at least one parameter column is contained.
#' Create a plotly scatter plot out of a benchmarkVis compatible data table.
#' The created scatter plot shows the connection between the specified measure and two different parameters.
#' x-Axis: the second parameter.
#' y-Axis: the first parameter.
#' Size and color of the circles: Depends on the measure value.
#'
#' @param dt compatible data table
#' @param measure the column name containing the results of a measure
#' @param parameter.column the parameter column which contains the parameter
#' @param parameter the first parameter you want to examine
#' @param parameter.column2 the parameter column which contains the second parameter
#' @param parameter2 the second parameter you want to examine
#' @param plot.area plot the measure as an area instead of scatters (default: FALSE)
#' @return a plotly scatter plot
#' @export
#' @examples
#' createParameterDualPlot(mlr.tuning.example, "measure.acc.test.mean", "algorithm.parameter", "C", "algorithm.parameter", "sigma")
createParameterDualPlot = function(dt,
  measure,
  parameter.column,
  parameter,
  parameter.column2,
  parameter2,
  plot.area = FALSE) {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_true(measure %in% getMeasures(dt))
  checkmate::assert_string(parameter.column)
  checkmate::assert_true(parameter.column %in% getParameterColumns(dt))
  checkmate::assert_string(parameter)
  checkmate::assert_string(parameter.column2)
  checkmate::assert_true(parameter.column2 %in% getParameterColumns(dt))
  checkmate::assert_string(parameter2)
  checkmate::assert_logical(plot.area)
  # Create new df as filtered dt
  new.df = dt[apply(dt, 1, function(x) {
    return(parameter %in% names(x[[parameter.column]]) &&
        parameter2 %in% names(x[[parameter.column2]]))
  }), ]
  checkmate::assert_true(nrow(new.df) > 0)
  # Get parameter for new data table
  new.df$param = sapply(new.df[[parameter.column]], function(x) {
    return(x[[parameter]])
  })
  new.df$param2 = sapply(new.df[[parameter.column2]], function(x) {
    return(x[[parameter2]])
  })
  new.df$me = new.df[[measure]]
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
      text = ~ paste0(
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
        me
      ),
      colors = c("green", "blue"),
      marker = list(colorbar = list(title = getPrettyMeasureName(measure)))
    )
  } else {
    p = plotly::plot_ly(
      new.df,
      x = ~ param2,
      y = ~ param,
      z = ~ me,
      type = "contour"
    )
    p = plotly::colorbar(p, title = getPrettyMeasureName(measure))
  }
  p = plotly::layout(
    p,
    xaxis = list(title = parameter2),
    yaxis = list(title = parameter)
  )
  return(p)
}

#' @title Create a parameter box plot
#'
#' @description
#' This plot is just usable if at least one parameter column is contained in the data table.
#' Create a plotly box plot out of a benchmarkVis compatible data table.
#' The created box chart shows the distribution for the input measure based on the specified parameter.
#' x-Axis: different values of the parameter.
#' y-Axis: the measure.
#'
#' @param dt campatible data table
#' @param measure the measure on the y axis
#' @param parameter.column the parameter column which contains the parameter
#' @param parameter the parameter you want to examine
#' @param violin if set to TRUE a violin plot instead of boxplot is produced (default: FALSE)
#' @return a plotly box plot
#' @export
#' @examples
#' createParameterBoxPlot(mlr.benchmark.example, "measure.mmce.test.mean", "problem.parameter", "size")
createParameterBoxPlot = function(dt,
  measure,
  parameter.column, parameter, violin = FALSE) {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_true(measure %in% getMeasures(dt))
  checkmate::assert_string(parameter.column)
  checkmate::assert_true(parameter.column %in% getParameterColumns(dt))
  checkmate::assert_string(parameter)
  checkmate::assert_logical(violin)
  # Create new df as filtered dt
  new.df = dt[apply(dt, 1, function(x) {
    return(parameter %in% names(x[[parameter.column]]))
  }), ]
  checkmate::assert_true(nrow(new.df) > 0)
  # Get parameter for new data table
  param = sapply(new.df[[parameter.column]], function(x) {
    return(x[[parameter]])
  })
  # Create plot
  if (violin) {
    geometry = ggplot2::geom_violin()
  } else {
    geometry = ggplot2::geom_boxplot()
  }
  p = ggplot2::ggplot(new.df, ggplot2::aes(
    x = as.factor(param),
    y = new.df[[measure]],
    fill = as.factor(param),
    colour = as.factor(param)
  )) +
    geometry + ggplot2::theme_bw() + ggplot2::labs(fill = parameter, colour = parameter)
  p = plotly::ggplotly(p)
  p = plotly::layout(
    p,
    xaxis = list(title = parameter),
    yaxis = list(title = getPrettyMeasureName(measure))
  )
  return(p)
}

#' @title Create a parameter density plot
#'
#' @description
#' This plot is just usable if at least one parameter column is contained in the data table.
#' Create a plotly density plot out of a benchmarkVis compatible data table.
#' The created density chart shows the distribution of the input measure based on the specified parameter..
#' x-Axis: the measure
#' y-Axis: the density.
#'
#' @param dt campatible data table
#' @param measure the input measure on the x axis
#' @param parameter.column the parameter column which contains the parameter
#' @param parameter the parameter you want to examine
#' @param stacked defines if the density curves should be stacked. Alternative is transparent (default: FALSE)
#' @return a plotly density plot
#' @export
#' @examples
#' createParameterDensityPlot(mlr.benchmark.example, "measure.mmce.test.mean", "problem.parameter", "size")
createParameterDensityPlot = function(dt, measure,
  parameter.column, parameter, stacked = FALSE) {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_true(measure %in% getMeasures(dt))
  checkmate::assert_string(parameter.column)
  checkmate::assert_true(parameter.column %in% getParameterColumns(dt))
  checkmate::assert_string(parameter)
  checkmate::assert_logical(stacked)
  # Create new df as filtered dt
  new.df = dt[apply(dt, 1, function(x) {
    return(parameter %in% names(x[[parameter.column]]))
  }), ]
  checkmate::assert_true(nrow(new.df) > 0)
  # Get parameter for new data table
  param = sapply(new.df[[parameter.column]], function(x) {
    return(x[[parameter]])
  })
  # Create plot
  p = ggplot2::ggplot(data = new.df, ggplot2::aes(new.df[[measure]], fill = as.factor(param))) + ggplot2::theme_bw() + ggplot2::labs(fill = parameter)
  if (stacked) {
    p = p + ggplot2::geom_density(position = "stack")
  } else {
    p = p + ggplot2::geom_density(alpha = 0.4)
  }
  # Convert plot to plotly
  p = plotly::ggplotly(p)
  p = plotly::layout(p, xaxis = list(title = getPrettyMeasureName(measure)))
  return(p)
}

#' @title Create a parameter measure matrix plot
#'
#' @description
#' This plot is just usable if at least one parameter column is contained in the data table.
#' Create a plotly measure matrix plot out of a benchmarkVis compatible data table.
#' The created measure matrix chart shows the distribution of each measure as density and scatter plot.
#'
#' @param dt campatible data table
#' @param parameter.column the parameter column which contains the parameter
#' @param parameter the parameter you want to examine
#' @return a plotly measure matrix plot
#' @export
#' @examples
#' createParameterMeasureMatrixPlot(mlr.tuning.example, "algorithm.parameter", "C")
createParameterMeasureMatrixPlot = function(dt, parameter.column, parameter) {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(parameter.column)
  checkmate::assert_true(parameter.column %in% getParameterColumns(dt))
  checkmate::assert_string(parameter)
  # Create new df as filtered dt
  new.df = dt[apply(dt, 1, function(x) {
    return(parameter %in% names(x[[parameter.column]]))
  }), ]
  checkmate::assert_true(nrow(new.df) > 0)
  # Get parameter for new data table
  new.df$param = as.factor(sapply(new.df[[parameter.column]], function(x) {
    return(x[[parameter]])
  }))
  # Create plot
  p = GGally::ggpairs(new.df, columns = getMeasures(new.df), columnLabels = sapply(getMeasures(new.df), getPrettyMeasureName), ggplot2::aes(colour = param)) +
    ggplot2::theme_bw()
  p = plotly::ggplotly(p)
  return(p)
}
