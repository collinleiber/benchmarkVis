#' @title Create a replication line plot
#'
#' @description
#' Create a plotly line plot out of a benchmarkVis compatible dataframe.
#' The created line chart shows the change within the specified measure for each iteration of the replication
#'
#' @param df campatible dataframe
#' @param replication.measure the column name containing the replications of a specific measure
#' @return a plotly line plot
#' @export
#' @examples
#' createReplicationLinePlot(microbenchmark.example, "replication.values")
createReplicationLinePlot = function(df, replication.measure) {
  checkmate::assert_data_frame(df)
  checkmate::assert_string(replication.measure)
  checkmate::assert_true(startsWith(replication.measure, "replication."))
  # Get maximum amount of replications
  max.iterations = max(sapply(df$replication.parameter, function(x) {
    return(x$iters)
  }))
  # Add NaN to fill problems with less iterations
  replications = lapply(df[[replication.measure]], function(x) {
    return(c(x, rep(NaN, max.iterations - length(x))))
  })
  # Create new plotly compatible dataframe
  new.df = data.frame(
    iteration = rep(1:max.iterations, nrow(df)),
    measure = unlist(replications),
    problem = rep(df$problem, rep(max.iterations, nrow(df))),
    algorithm = rep(df$algorithm, rep(max.iterations, nrow(df)))
  )
  # Create plot
  p = ggplot2::ggplot(data = new.df, ggplot2::aes(
    x = iteration,
    y = measure,
    group = interaction(problem, algorithm),
    colour = algorithm
  )) + ggplot2::geom_point() + ggplot2::geom_line()
  # Convert plot to plotly
  return(plotly::ggplotly(p))
}
