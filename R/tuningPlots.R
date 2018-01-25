#' @title Create a tuning parameter scatter plot
#'
#' @description
#' Create a plotly scatter plot out of a benchmarkVis compatible dataframe.
#' The created scatter plot shows the change within the specified measure for a change of the specified parameter
#'
#' @param df campatible dataframe
#' @param parameter the parameter you want to examine
#' @param measure the column name containing the results of a measure
#' @return a plotly scatter plot
#' @export
#' @examples
#' createTuningParameterPlot(mlr.tuning.example, "C", "acc.test.mean")
createTuningParameterPlot = function(df, parameter, measure) {
  checkmate::assert_data_frame(df)
  checkmate::assert_string(parameter)
  checkmate::assert_string(measure)
  # Get parameter for new dataframe
  param = sapply(df$algorithm.parameter, function(x) {
    return(x[[parameter]])
  })
  # Create new plotly compatible dataframe
  new.df = data.frame(
    parameter = param,
    measure = df[[measure]]
  )
  # Create plot
  p = plotly::plot_ly(new.df, x = ~parameter, y = ~measure, type = "scatter", mode = "markers")
  p = plotly::layout(p, yaxis = list(title = measure), xaxis = list(title = parameter))
  return(p)
}

#' @title Create a tuning iteration line plot
#'
#' @description
#' Create a plotly line plot out of a benchmarkVis compatible dataframe.
#' The created line plot shows the change within the specified measure for each iteration
#'
#' @param df campatible dataframe
#' @param measure the column name containing the results of a measure
#' @return a plotly line plot
#' @export
#' @examples
#' createTuningIterationPlot(mlr.tuning.example, "acc.test.mean")
createTuningIterationPlot = function(df, measure) {
  checkmate::assert_string(measure)
  checkmate::assert_data_frame(df)
  # Get parameter for new dataframe
  iter = sapply(df$algorithm.parameter, function(x) {
    return(x$iteration)
  })
  param = sapply(df$algorithm.parameter, function(x) {
    return(toString(paste(names(x)[-1], x[-1], sep = " = ")))
  })
  # Create new plotly compatible dataframe
  new.df = data.frame(
    iteration = iter,
    measure = df[[measure]],
    parameter = param
  )
  # Create plot
  p = plotly::plot_ly(new.df, x = ~iteration, y = ~measure, type = "scatter", text = ~parameter, mode = "lines+markers")
  p = plotly::layout(p, yaxis = list(title = measure))
  return(p)
}
