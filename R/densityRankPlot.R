#' @title Create a density rank plot
#'
#' @description
#' Create a plotly sensity plot out of a benchmarkVis compatible data table.
#' The created plot shows the distribution of the ranks each input achieves depending on a group and measures.
#' x-Axis: the ranks.
#' y-Axis: the density.
#'
#' @param dt compatible data table
#' @param ignore.measures a vector of strings describing which measures to leave out of the plot (default: empty)
#' @param stack.plots defines if the density curves should be stacked. Alternative is transparent. Default: FALSE
#' @param color.by the column to color the input with. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @param group.by the column to group the ranks by. Possibilities: "algorithm", "problem", "replication" (default: "problem")
#' @return a plotly density rank plot
#' @export
#' @examples
#' createDensityRankPlot(mlr.benchmark.example, c("measure.timetrain.test.mean","measure.mmce.test.mean"))
createDensityRankPlot = function(dt, ignore.measures = vector(), stack.plots = FALSE, color.by = "algorithm", group.by = "problem") {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_character(ignore.measures)
  checkmate::assert_true(all(ignore.measures %in% getMeasures(dt)))
  checkmate::assert_string(color.by)
  checkmate::assert_string(group.by)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  checkmate::assert_true(group.by %in% getMainColumns(dt))
  # Create new data frame with all ranks
  new.df = data.frame(color = rep(dt[[color.by]], getMeasuresCount(dt) - length(ignore.measures)))
  # Get ranks for each measure and each group
  ranks = vector()
  for (measure in getMeasures(dt)) {
    if (!measure %in% ignore.measures) {
      for (p in levels(dt[[group.by]])) {
        ranks = c(ranks, rank(dt[dt[[group.by]] == p, ][[measure]], ties.method = "min"))
      }
    }
  }
  new.df$ranks = ranks
  # Create plot
  p = ggplot2::ggplot(data = new.df, ggplot2::aes(ranks, fill = color))  + ggplot2::theme_bw()
  if (stack.plots) {
    p = p + ggplot2::geom_density(position = "stack")
  } else {
    p = p + ggplot2::geom_density(alpha = 0.4)
  }
  p = p + ggplot2::labs(fill = color.by)
  # Convert plot to plotly
  p = plotly::ggplotly(p)
  p = plotly::layout(p, xaxis = list(title = "ranks"))
  return(p)
}
