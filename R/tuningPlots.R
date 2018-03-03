#' @title Create a tuning parameter scatter plot
#'
#' @description
#' Create a plotly scatter plot out of a benchmarkVis compatible data table
#' The created scatter plot shows the change within the specified measure for a change of the specified parameter
#'
#' @param dt compatible data table
#' @param parameter the parameter you want to examine
#' @param measure the column name containing the results of a measure
#' @return a plotly scatter plot
#' @export
#' @examples
#' createTuningParameterPlot(mlr.tuning.example, "C", "measure.acc.test.mean")
createTuningParameterPlot = function(dt, parameter, measure) {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(parameter)
  checkmate::assert_string(measure)
  checkmate::assert_true(startsWith(measure, "measure."))
  # Get parameter for new data table
  param = sapply(dt$algorithm.parameter, function(x) {
    return(x[[parameter]])
  })
  # Create new plotly compatible data frame
  new.df = data.frame(
    parameter = param,
    measure = dt[[measure]]
  )
  # Create plot
  p = plotly::plot_ly(new.df, x = ~parameter, y = ~measure, type = "scatter", mode = "markers")
  p = plotly::layout(p, yaxis = list(title = measure), xaxis = list(title = parameter))
  return(p)
}

#' @title Create a tuning iteration line plot
#'
#' @description
#' Create a plotly line plot out of a benchmarkVis compatible data table.
#' The created line plot shows the change within the specified measure for each iteration
#'
#' @param dt compatible data table
#' @param measure the column name containing the results of a measure
#' @return a plotly line plot
#' @export
#' @examples
#' createTuningIterationPlot(mlr.tuning.example, "measure.acc.test.mean")
createTuningIterationPlot = function(dt, measure) {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_true(startsWith(measure, "measure."))
  # Get parameter for new data table
  iter = sapply(dt$algorithm.parameter, function(x) {
    return(x$iteration)
  })
  param = sapply(dt$algorithm.parameter, function(x) {
    return(toString(paste(names(x)[-1], x[-1], sep = " = ")))
  })
  # Create new plotly compatible data frame
  new.df = data.frame(
    iteration = iter,
    measure = dt[[measure]],
    parameter = param
  )
  # Create plot
  p = plotly::plot_ly(new.df, x = ~iteration, y = ~measure, type = "scatter", text = ~parameter, mode = "lines+markers")
  p = plotly::layout(p, yaxis = list(title = measure))
  return(p)
}
