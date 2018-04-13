#' @title Create a measure matrix plot
#'
#' @description
#' Create a measure matrix plot out of a benchmarkVis compatible data table.
#' The created matrix plot shows all measure results for each benchmark entry.
#'
#' @param dt compatible data table
#' @param color.by the column to color the markers with. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @return a measure matrix plot
#' @export
#' @examples
#' createMeasureMatrixPlot(mlr.benchmark.example)
createMeasureMatrixPlot = function(dt, color.by = "algorithm") {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(color.by)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  # Create plot
  p = GGally::ggpairs(dt, columns = getMeasures(dt), columnLabels = sapply(getMeasures(dt), getPrettyMeasureName), ggplot2::aes_string(colour = color.by)) + ggplot2::theme_bw()
  p = plotly::ggplotly(p)
  return(p)
}
