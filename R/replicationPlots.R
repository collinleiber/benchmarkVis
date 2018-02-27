#' @title Create a replication line plot
#'
#' @description
#' Create a plotly line plot out of a benchmarkVis compatible data table.
#' The created line chart shows the change within the specified measure for each iteration of the replication
#'
#' @param dt campatible data table
#' @param list.measure the column name containing the replications of a specific measure
#' @return a plotly line plot
#' @export
#' @examples
#' createReplicationLinePlot(microbenchmark.example, "list.values")
createReplicationLinePlot = function(dt, list.measure) {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(list.measure)
  checkmate::assert_true(startsWith(list.measure, "list."))
  # Get maximum amount of replications
  max.iterations = max(sapply(dt$replication.parameter, function(x) {
    return(x$iters)
  }))
  # Add NaN to fill problems with less iterations
  replications = lapply(dt[[list.measure]], function(x) {
    return(c(x, rep(NaN, max.iterations - length(x))))
  })
  # Create new plotly compatible data table
  new.df = data.frame(
    iteration = rep(1:max.iterations, nrow(dt)),
    measure = unlist(replications),
    problem = rep(dt$problem, rep(max.iterations, nrow(dt))),
    algorithm = rep(dt$algorithm, rep(max.iterations, nrow(dt)))
  )
  # Create plot
  p = ggplot2::ggplot(data = new.df, ggplot2::aes(
    x = iteration,
    y = measure,
    group = interaction(problem, algorithm),
    colour = algorithm
  )) + ggplot2::geom_point() + ggplot2::geom_line()
  # Convert plot to plotly
  p = plotly::ggplotly(p)
  p = plotly::layout(p, yaxis = list(title = list.measure))
  return(p)
}
