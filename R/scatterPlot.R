#' @title Create a scatter plot
#'
#' @description
#' Create a scatter plot out of a benchmarkVis compatible data table.
#' The created scatter plot allows to compare the performances different algorithms based on the giving performance measure.
#' x-Axis: problems.
#' y-Axis: measure.
#' color: the algorithm.
#'
#' @param dt compatible data table
#' @param measure the measure to plot
#' @return a scatter plot
#' @export
#' @examples
#' createScatterPlot(mlr.benchmark.example, 'measure.mmce.test.mean')
createScatterPlot = function(dt, measure) {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_true(measure %in% getMeasures(dt))
  # Create new data frame
  new.df = data.frame(
    measure = dt[[measure]],
    problem = dt$problem,
    algorithm = dt$algorithm
  )
  # Create Plot
  p = plotly::plot_ly(new.df, x = ~problem, y = ~ measure, color = ~algorithm, type = "scatter", mode = "markers", marker = list(size = 10))
  p = plotly::layout(p, xaxis = list(title = "problem"), yaxis = list(title = measure), margin = list(b = 100))
  return(p)
}
