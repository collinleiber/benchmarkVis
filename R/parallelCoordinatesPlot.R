#' @title Create a parallel coordinates plot
#'
#' @description
#' Create a plotly parallel coordinates plot out of a benchmarkVis compatible data table.
#' The created parallel coordinates plot shows the relationship between problem, algorithm and all measures.
#'
#' @param dt compatible data table
#' @return a plotly parallel coordinates plot
#' @export
#' @examples
#' createParallelCoordinatesPlot(mlr.benchmark.example)
createParallelCoordinatesPlot = function(dt) {
  # TODO: Works just in the browser at the moment!!! =======
  # options(viewer = NULL)
  # ========================================================
  # Add algorithm and problem coordinates
  dim = list(
    list(
      range = c(1, length(unique(dt$problem))),
      tickvals = seq(length(unique(dt$problem))),
      label = "problem",
      values = sapply(dt$problem, function(x) {
        return(which(unique(dt$problem) == x))
      }),
      ticktext = unique(dt$problem)
    ),
    list(
      range = c(1, length(unique(dt$algorithm))),
      tickvals = seq(length(unique(dt$algorithm))),
      label = "algorithm",
      values = sapply(dt$algorithm, function(x) {
        return(which(unique(dt$algorithm) == x))
      }),
      ticktext = unique(dt$algorithm)
    )
  )
  # Add measure coordinates
  for (i in 1:getMeasuresCount(dt)) {
    measure = getMeasures(dt)[i]
    # Construct parallel coordinates compatible list
    tmp = list(range = c(min(dt[[measure]]), max(dt[[measure]])),
      label = measure,
      values = dt[[measure]])
    dim[[i + 2]] = tmp
  }
  # Create plot
  p = plotly::plot_ly(
    type = "parcoords",
    line = list(
      # Make color depend on the algorithm
      color = sapply(dt$algorithm, function(x) {
        return(which(unique(dt$algorithm) == x))
      }),
      colorscale = list(c(0, "red"), c(0.5, "green"), c(1, "blue"))
    ),
    dimensions = dim
  )
  return(p)
}
