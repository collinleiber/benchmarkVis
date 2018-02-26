#' @title Create a box plot
#'
#' @description
#' Create a plotly box plot out of a benchmarkVis compatible data table.
#' The created box plot allows for comparison of algorithms based on a given performance measure for different datasets
#'
#' @param dt compatible data table
#' @param measure measure for comparison
#' @return a box plot
#' @export
#' @examples
#' createBoxPlot(mlr.benchmark.example, 'measure.mmce.test.mean')
createBoxPlot = function(dt, measure) {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_true(startsWith(measure, "measure."))

  p = plotly::plot_ly(dt, x = ~algorithm, y = ~dt[, measure], color = ~algorithm, type = "box")
  return(p)
}
