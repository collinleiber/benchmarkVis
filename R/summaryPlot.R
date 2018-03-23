#' @title Create a summary plot
#'
#' @description
#' Create a partially filled bar charts plot out of a benchmarkVis compatible data table.
#' each line refers to a task, On that line the aggregated scores for all learners are plotted.
#' The created partially bar charts allow to compare the performance of every combination of task and learner
#' x-Axis: the measure.
#' y-Axis: the problem.
#' color: the algorithem.
#' @param dt compatible data table
#' @param measure measure for comparison
#' @return a summary plot
#' @export
#' @examples
#' createSummaryPlot(mlr.benchmark.example, 'measure.mmce.test.mean')
createSummaryPlot = function(dt, measure){
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  abc = max(dt[[measure]], na.rm = TRUE)
  copy = cbind(dt)
  pos.fun =  function(y) {
    y = seq(0, length(y) - 1)
  }
  copy$Nor = ave(copy[[measure]], copy$problem, FUN = function(x) x / max(x))

  copy$Pos = ave(copy$Nor, copy$problem, FUN = pos.fun)
  p = ggplot2::ggplot(dt, ggplot2::aes(x = dt$problem, fill = dt$algorithm)) +
    ggplot2::geom_bar(ggplot2::aes(y = copy$Nor), stat = "identity", position = ggplot2::position_nudge(y = copy$Pos)) +
    ggplot2::geom_col(ggplot2::aes(y = 1), alpha = .1, data = dt) +
    ggplot2::labs(title = "Summary Plot",
                  y = "mmce proportional to best performance",
                  x = "problems",
      fill = "algorithms")
  p = p + ggplot2::coord_flip()
  #there may be some bug about the ggplot2, so be sure you have installed 'hadley/ggplot2'.
  #otherwise, there some may get a unexpected polt.
  p = plotly::ggplotly(p)
  #p = plotly::layout(p, yaxis = list(copy$Nor))
  return(p)
}
