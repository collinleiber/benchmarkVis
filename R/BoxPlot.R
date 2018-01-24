#' @title Create a box plot
#'
#' @description
#' Create a plotly box plot out of a benchmarkVis compatible dataframe.
#' The created box plot allows for comparison of algorithms based on a given performance measure for different datasets
#'
#' @param df compatible dataframe
#' @param measure measure for comparison
#' @return a box plot
#' @export
#' @examples
#' createBoxPlot(mlr.benchmark.example, 'mmce.test.mean')
createBoxPlot = function(df, measure) {
  checkmate::assert_data_frame(df)

  p = plotly::plot_ly(df, x = ~algorithm, y = ~df[,measure], color = ~algorithm, type = "box")
  return(p)
}
