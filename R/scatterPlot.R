#' @title Create a scatter plot
#'
#' @description
#' Create a scatter plot out of a benchmarkVis compatible data table.
#' The created scatter plot allows to compare the performances based on the giving performance measure.
#' x-Axis: the groups.
#' y-Axis: measure.
#'
#' @param dt compatible data table
#' @param measure the measure to plot
#' @param color.by the column to color the markers with. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @param group.by the column to group the markers by. Possibilities: "algorithm", "problem", "replication" (default: "problem")
#' @return a scatter plot
#' @export
#' @examples
#' createScatterPlot(mlr.benchmark.example, "measure.mmce.test.mean")
createScatterPlot = function(dt, measure, color.by = "algorithm", group.by = "problem") {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_string(color.by)
  checkmate::assert_string(group.by)
  checkmate::assert_true(measure %in% getMeasures(dt))
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  checkmate::assert_true(group.by %in% getMainColumns(dt))
  # Create new data frame
  new.df = data.frame(
    measure = dt[[measure]],
    group = dt[[group.by]],
    color = dt[[color.by]]
  )
  # Create Plot
  p = plotly::plot_ly(new.df, x = ~group, y = ~ measure, color = ~color, type = "scatter", mode = "markers", marker = list(size = 10))
  p = plotly::layout(p, xaxis = list(title = group.by), yaxis = list(title = getPrettyMeasureName(measure)), margin = list(b = 100))
  return(p)
}
