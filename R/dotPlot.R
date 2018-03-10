#' @title Create a dot plot
#'
#' @description
#' Create a scatter plot out of a benchmarkVis compatible data table.
#' The created scatter plot allows to compare the performance of interaction(algorithm and task) based on two giving performance measures for different datasets.
#' x-Axis: measure1.
#' y-Axis: measure2.
#' color: the interaction of algorithm and problem.
#' @param dt compatible data table
#' @param measure1 the first measure for comparison
#' @param measure2 the second measure  for comparison
#' @param pointsize the size of ploted point
#' @param jitter small vertical jitter to deal with overplotting in case of equal scores
#' @return a dot scatter plot
#' @export
#' @examples
#' createDotPlot(mlr.benchmark.example, 'measure.mmce.test.mean','measure.ber.test.mean')
createDotPlot = function(dt, measure1, measure2, pointsize = 4L, jitter = 1) {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure1)
  checkmate::assert_string(measure2)
  p = ggplot2::ggplot(dt, ggplot2::aes_string(x = measure1, y = measure2, col = interaction(dt$problem, dt$algorithm, sep = "-", lex.order = TRUE)))
  p = p + ggplot2::geom_point(size = pointsize, position = ggplot2::position_jitter(width = 0, height = jitter))
  p = p + ggplot2::ylab(measure1)
  p = p + ggplot2::xlab(measure2)
  return(p)
}
