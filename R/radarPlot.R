#' @title Create a radar plot
#'
#' @description
#' Create a radar plot out of a benchmarkVis compatible data table.
#' The created radar plot shows all measure results for each benchmark entry.
#'
#' @param dt compatible data table
#' @return a radar plot
#' @export
#' @examples
#' createRadarPlot(microbenchmark.example)
createRadarPlot = function(dt) {
  # Checks
  checkmate::assert_data_table(dt)
  # Create new data frame
  scores = data.frame("Label" = sapply(getMeasures(dt), getPrettyMeasureName))
  for (row in seq(nrow(dt))) {
    vec = vector()
    for (measure in getMeasures(dt)) {
      vec = c(vec, dt[[row, measure]])
    }
    if ("replication" %in% getMainColumns(dt)) {
      scores[[paste(dt[[row, "problem"]], dt[[row, "algorithm"]], dt[[row, "replication"]], sep = ".")]] = vec
    } else {
      scores[[paste(dt[[row, "problem"]], dt[[row, "algorithm"]], sep = ".")]] = vec
    }
  }
  # Create plot
  p = radarchart::chartJSRadar(scores)
  return(p)
}
