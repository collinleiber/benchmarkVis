#' @title Create a rank plot
#'
#' @description
#' Create a plotly rank plot out of a benchmarkVis compatible data table.
#' The created rank plot allows for Ranking of algorithms based on a given performance measure for different datasets
#'
#' @param dt compatible data table
#' @param measure measure for Ranking
#' @return a rank plot
#' @export
#' @examples
#' createRankPlot(mlr.benchmark.example, 'measure.mmce.test.mean')
createRankPlot = function(dt, measure) {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_true(startsWith(measure, "measure."))
  if (length(dt[, measure])) {

    `%>%` = magrittr::`%>%`

    abcd = dt %>% dplyr::group_by(dt$problem) %>% dplyr::mutate(rank = order(eval(parse(text = sprintf("%s", measure)))))

    order.scores = order(dt$problem, dt[, measure])
    rank = NA
    rank[order.scores] = seq_len(nrow(dt))
    rank = as.factor(rank)
    p =  ggplot2::ggplot(abcd, ggplot2::aes(x = rank, problem, fill = algorithm)) +
      ggplot2::geom_tile() +
      ggplot2::labs(title = "Rank Plot",
                    x = "Rank",
                    y = "Task.id")
    return(p)
  }
}
