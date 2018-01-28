#' @title Create a rank plot
#'
#' @description
#' Create a plotly rank plot out of a benchmarkVis compatible dataframe.
#' The created rank plot allows for Ranking of algorithms based on a given performance measure for different datasets
#'
#' @param df compatible dataframe
#' @param measure measure for Ranking
#' @return a rank plot
#' @export
#' @import ggplot2
#' @import dplyr
#' @examples
#' createRankPlot(mlr.benchmark.example, 'mmce.test.mean')
createRankPlot = function(df, measure) {
  checkmate::assert_data_frame(df)
  if (length(df[, measure])) {

    abcd = df %>% group_by(df$problem) %>% mutate(rank = order(eval(parse(text = sprintf("%s", measure)))))

    order.scores = order(df$problem, df[, measure])
    rank = NA
    rank[order.scores] = seq_len(nrow(df))
    rank = as.factor(rank)
    p =  ggplot2::ggplot(abcd, ggplot2::aes(x = rank, problem, fill = algorithm)) +
      ggplot2::geom_tile() +
      ggplot2::labs(title = "Rank Plot",
                    x = "Rank",
                    y = "Task.id")
    return(p)
  }
}
