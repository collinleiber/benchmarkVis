#' @title Create a density plot
#'
#' @description
#' Create a plotly density plot out of a benchmarkVis compatible data table.
#' The created density chart shows the distribution of the specified measure for each algorithm
#'
#' @param dt campatible data table
#' @param measure the column name of the measure
#' @param stack.plots defines if the density curves should be stacked. Alternative is transparent. Default: FALSE
#' @return a plotly density plot
#' @export
#' @examples
#' createDensityPlot(mlr.benchmark.example, "measure.mmce.test.mean", TRUE)
createDensityPlot = function(dt, measure, stack.plots = FALSE) {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_logical(stack.plots)
  checkmate::assert_true(measure %in% getMeasures(dt))
  # create new dataframe
  new.df = data.frame(
    algorithm = dt$algorithm,
    meas = dt[[measure]]
  )
  # Create plot
  p = ggplot2::ggplot(data = new.df, ggplot2::aes(meas, fill = algorithm))  + ggplot2::theme_bw()
  if (stack.plots) {
    p = p + ggplot2::geom_density(position = "stack")
  } else {
    p = p + ggplot2::geom_density(alpha = 0.4)
  }
  # Convert plot to plotly
  p = plotly::ggplotly(p)
  p = plotly::layout(p, xaxis = list(title = measure))
  return(p)
}
