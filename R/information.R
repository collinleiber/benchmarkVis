#' @title Get list of all possible plots
#'
#' @description
#' Get a complete list of all plots implemented within the benchmarkVis application
#'
#' @return vector containing all possbile plots
#' @export
listPlots = function() {
  plots = c("boxPlot",
    "replicationLinePlot")
  return(plots)
}

#' @title Get list of all possible wrappers
#'
#' @description
#' Get a complete list of all wrappers implemented within the benchmarkVis application
#'
#' @return vector containing all possbile wrappers
#' @export
listWrappers = function() {
  wrapper = c("csvHandler",
    "microbenchmarkWrapper",
    "mlrBenchmarkWrapper",
    "mlrTuningWrapper",
    "rbenchmarkWrapper")
  return(wrapper)
}
