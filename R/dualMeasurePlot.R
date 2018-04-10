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
#' @param regression.line add a regression line to the plot (default: FALSE)
#' @return a dual measure plot
#' @export
#' @examples
#' createDualMeasurePlot(mlr.benchmark.example, "measure.mmce.test.mean","measure.ber.test.mean")
createDualMeasurePlot = function(dt, measure1, measure2, color.by = "algorithm", interaction.column = "none", regression.line = FALSE) {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_logical(regression.line)
  checkmate::assert_string(measure1)
  checkmate::assert_true(measure1 %in% getMeasures(dt))
  checkmate::assert_string(measure2)
  checkmate::assert_true(measure2 %in% getMeasures(dt))
  checkmate::assert_string(color.by)
  checkmate::assert_string(interaction.column)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  checkmate::assert_true(interaction.column %in% getMainColumns(dt) | interaction.column == "none")
  # Get data
  if (interaction.column != "none") {
    col.op = interaction(dt[[color.by]], dt[[interaction.column]])
  } else {
    col.op = dt[[color.by]]
  }
  # Get text
  if ("replication" %in% getMainColumns(dt)) {
    text = paste("problem: ", dt$problem, "<br>algorithm: ", dt$algorithm, "<br>replication: ", dt$replication, sep = "")
  } else {
    text = paste("problem: ", dt$problem, "<br>algorithm: ", dt$algorithm, sep = "")
  }
  # Create plot
  p = plotly::plot_ly(dt)
  p = plotly::add_trace(p, x = dt[[measure1]], y = dt[[measure2]], color = col.op, text = text, type = "scatter", mode = "markers")
  # Add regression line
  if (regression.line) {
    fit = lm(dt[[measure2]] ~ dt[[measure1]])
    p = plotly::add_lines(p, x = dt[[measure1]], y = fitted(fit), name = "regression")
  }
  p = plotly::layout(p, xaxis = list(title = measure1), yaxis = list(title = measure2))
  return(p)
}
