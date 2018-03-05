#' @title Create a box plot
#'
#' @description
#' Create a plotly box plot out of a benchmarkVis compatible data table.
#' The created box plot allows for comparison of algorithms based on a given performance measure for different datasets
#'
#' @param dt compatible data table
#' @param measure measure for comparison
#' @param violin if set to TRUE a violin plot instead of boxplot is produced (facetting_problem should be FALSE)
#' @param facetting_problem if set to TRUE boxplot with facet wrap is produced (violin should be FALSE)
#' @return a box plot
#' @export
#' @examples
#' createBoxPlot(mlr.benchmark.example, 'measure.mmce.test.mean')
#' createBoxPlot(mlr.benchmark.example, 'measure.mmce.test.mean', violin = TRUE)
#' createBoxPlot(mlr.benchmark.example, 'measure.mmce.test.mean', facetting_problem = TRUE)
createBoxPlot = function(dt, measure, violin = FALSE, facetting_problem = FALSE) {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_logical(violin)
  checkmate::assert_logical(facetting_problem)
  checkmate::assert_true(startsWith(measure, "measure."))
  if (violin) checkmate::assert_false(facetting_problem)
  if (facetting_problem) checkmate::assert_false(violin)
  measure.short = strsplit(measure, "measure.")[[1]][2]
  if (violin) geometry = ggplot2::geom_violin()
  else geometry = ggplot2::geom_boxplot()
  p = ggplot2::ggplot(dt, ggplot2::aes(x = algorithm, y = dt[, measure], fill = algorithm, colour = algorithm)) +
    geometry
  if (facetting_problem) p = p + ggplot2::facet_wrap(~problem, ncol = 2)
  p = plotly::ggplotly(p)
  p = plotly::layout(
    p,
    xaxis = list(title = "algorithm"),
    yaxis = list(title = measure.short)
  )
  return(p)
}

