#' @title Create a box plot
#'
#' @description
#' Create a plotly box plot out of a benchmarkVis compatible data table.
#' The created box plot allows for comparison of the by color.by specified input based on a given performance measure
#'
#' @param dt compatible data table
#' @param measure measure for comparison
#' @param violin if set to TRUE a violin plot instead of boxplot is produced (default: FALSE)
#' @param color.by the column to color the density area with. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @return a box plot
#' @export
#' @examples
#' createBoxPlot(mlr.benchmark.example, 'measure.mmce.test.mean')
#' createBoxPlot(mlr.benchmark.example, 'measure.mmce.test.mean', violin = TRUE)
createBoxPlot = function(dt, measure, violin = FALSE, color.by = "algorithm") {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_logical(violin)
  checkmate::assert_true(measure %in% getMeasures(dt))
  checkmate::assert_string(color.by)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  # Create Plot
  if (violin) {
    geometry = ggplot2::geom_violin()
  } else {
    geometry = ggplot2::geom_boxplot()
  }
  p = ggplot2::ggplot(dt, ggplot2::aes(x = dt[[color.by]], y = dt[[measure]], fill = dt[[color.by]], colour = dt[[color.by]])) +
    geometry + ggplot2::labs(fill = color.by, col = color.by) + ggplot2::theme_bw()
  p = plotly::ggplotly(p)
  p = plotly::layout(
    p,
    xaxis = list(title = color.by),
    yaxis = list(title = getPrettyMeasureName(measure))
  )
  return(p)
}
