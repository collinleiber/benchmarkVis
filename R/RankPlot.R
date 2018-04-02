#' @title Create a rank plot
#'
#' @description
#' Create a plotly rank plot out of a benchmarkVis compatible data table.
#' The created rank plot allows for Ranking of the by color.by specified input based on a given performance measure
#'
#' @param dt compatible data table
#' @param measure measure for Ranking
#' @param color.by the column to color the markers with. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @param group.by the column to group the markers by. Possibilities: "algorithm", "problem", "replication" (default: "problem")
#' @return a rank plot
#' @export
#' @examples
#' createRankPlot(mlr.benchmark.example, "measure.mmce.test.mean")
createRankPlot = function(dt, measure, color.by = "algorithm", group.by = "problem") {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_true(measure %in% getMeasures(dt))
  checkmate::assert_string(color.by)
  checkmate::assert_string(group.by)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  checkmate::assert_true(group.by %in% getMainColumns(dt))
  # Create data
  `%>%` = magrittr::`%>%`
  abcd = dt %>% dplyr::group_by(dt[[group.by]]) %>% dplyr::mutate(rank = order(eval(parse(
    text = sprintf("%s", measure)
  ))))
  order.scores = order(dt[[group.by]], dt[[measure]])
  rank = NA
  rank[order.scores] = seq_len(nrow(dt))
  rank = as.factor(rank)
  # Create plot
  p =  ggplot2::ggplot(abcd, ggplot2::aes(x = rank, abcd[[group.by]], fill = abcd[[color.by]])) +
    ggplot2::geom_tile() + ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Rank",
      y = group.by,
      fill = color.by)
  p = plotly::ggplotly(p)
  return(p)
}
