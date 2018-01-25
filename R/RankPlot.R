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
#' @examples
#' createRankPlot(mlr.benchmark.example, 'mmce.test.mean')
createRankPlot = function(df, measure, problem, algorithm) {
  checkmate::assert_data_frame(df)
  if (length(df[, measure])) {
        order.scores<-order(df[, measure])
        df$rank <- NA
        df$rank[order.scores] <- 1:nrow(df)
        df$rank = as.factor(df$rank)
        p =  ggplot2::ggplot(df, ggplot2::aes(x = df$rank, problem, fill = algorithm))+
             ggplot2::geom_tile(position = "dodge") +
             ggplot2::labs(title= "Rank Plot",
               x= "Rank",
               y= "Task.id")
            #  return(p)
        return(p)
  }
}
