#' @title Create a summary plot
#'
#' @description
#' Create a partially filled bar charts plot out of a benchmarkVis compatible data table.
#' Each line refers to a group specified by group.by. On that line the aggregated scores for the color.by inputs are plotted.
#' The created partially bar charts allow to compare the performance of every combination.
#' x-Axis: the measure.
#' y-Axis: the groups.
#'
#' @param dt compatible data table
#' @param measure measure for comparison
#' @param color.by the column to color the bars with. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @param group.by the column to group the bars by. Possibilities: "algorithm", "problem", "replication" (default: "problem")
#' @return a summary plot
#' @export
#' @examples
#' createSummaryPlot(mlr.benchmark.example, 'measure.mmce.test.mean')
createSummaryPlot = function(dt, measure, color.by = "algorithm", group.by = "problem"){
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_true(measure %in% getMeasures(dt))
  checkmate::assert_string(color.by)
  checkmate::assert_string(group.by)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  checkmate::assert_true(group.by %in% getMainColumns(dt))
  # New dataframe
  abc = max(dt[[measure]], na.rm = TRUE)
  copy = cbind(dt)
  pos.fun =  function(y) {
    y = seq(0, length(y) - 1)
  }
  copy$Nor = ave(copy[[measure]], copy[[group.by]], FUN = function(x) x / max(x))
  copy$Pos = ave(copy$Nor, copy[[group.by]], FUN = pos.fun)
  # Create plot
  p = ggplot2::ggplot(dt, ggplot2::aes(x = dt[[group.by]], fill = dt[[color.by]])) +
    ggplot2::geom_bar(ggplot2::aes(y = copy$Nor), stat = "identity", position = ggplot2::position_nudge(y = copy$Pos)) +
    ggplot2::geom_col(ggplot2::aes(y = 1), alpha = .1, data = dt) +
    ggplot2::labs(y = getPrettyMeasureName(measure),
                  x = group.by,
      fill = color.by)
  p = p + ggplot2::coord_flip() + ggplot2::theme_bw()
  #there may be some bug about the ggplot2, so be sure you have installed 'hadley/ggplot2'.
  #otherwise, there some may get a unexpected polt.
  p = plotly::ggplotly(p)
  return(p)
}
