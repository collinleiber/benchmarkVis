#' @title Create a Dual Measure plot
#'
#' @description
#' Create a Dual Measure plot out of a benchmarkVis compatible data table.
#' The created Dual Measure plot allows to compare the performance of the color.by input based on two giving performance measures.
#' x-Axis: measure1.
#' y-Axis: measure2.
#'
#' @param dt compatible data table
#' @param measure1 the first measure for comparison
#' @param measure2 the second measure  for comparison
#' @param color.by the column to color the markers with. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @param interaction.column will result in colors being an interaction of color.by and this value. Possibilities: "algorithm", "problem", "replication", "none" (default: "none")
#' @param pointsize the size of ploted point
#' @param jitter small vertical jitter to deal with overplotting in case of equal scores
#' @return a dual measure plot
#' @export
#' @examples
#' createDualMeasurePlot(mlr.benchmark.example, 'measure.mmce.test.mean','measure.ber.test.mean')
createDualMeasurePlot = function(dt, measure1, measure2, color.by = "algorithm", interaction.column = "none", pointsize = 4L, jitter = 0) {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure1)
  checkmate::assert_true(measure1 %in% getMeasures(dt))
  checkmate::assert_string(measure2)
  checkmate::assert_true(measure2 %in% getMeasures(dt))
  checkmate::assert_string(color.by)
  checkmate::assert_string(interaction.column)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  checkmate::assert_true(interaction.column %in% getMainColumns(dt) | interaction.column == "none")
  checkmate::assert_numeric(pointsize)
  checkmate::assert_numeric(jitter)
  # Get data
  if (interaction.column != "none") {
    col.op = interaction(dt[[color.by]], dt[[interaction.column]])
  } else {
    col.op = dt[[color.by]]
  }
  # Create plot
  p = ggplot2::ggplot(dt, ggplot2::aes_string(x = measure1, y = measure2, col = col.op)) + ggplot2::theme_bw()
  p = p + ggplot2::geom_point(size = pointsize, position = ggplot2::position_jitter(width = 0, height = jitter))
  p = p + ggplot2::ylab(measure1)
  p = p + ggplot2::xlab(measure2)
  if (interaction.column != "none") {
    p = p + ggplot2::labs(col = paste(color.by, interaction.column, sep = "."))
  } else {
    p = p + ggplot2::labs(col = color.by)
  }
  p = plotly::ggplotly(p)
  return(p)
}
