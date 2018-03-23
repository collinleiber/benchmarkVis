# Startup message
.onAttach = function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to the benchmarkVis package. For instructions visit the wiki at https://github.com/collinleiber/benchmarkVis/wiki"
  )
}

#' @title Get list of all possible plots
#'
#' @description
#' Get a complete list of all plots implemented within the benchmarkVis application
#'
#' @return vector containing all possbile plots
#' @export
listPlots = function() {
  methods = lsf.str("package:benchmarkVis")
  condition = sapply(methods, function(x) {
    startsWith(x, "create") && endsWith(x, "Plot")
  })
  return(methods[condition])
}

#' @title Get list of all possible wrappers
#'
#' @description
#' Get a complete list of all wrappers implemented within the benchmarkVis application
#'
#' @return vector containing all possbile wrappers
#' @export
listWrappers = function() {
  methods = lsf.str("package:benchmarkVis")
  condition = sapply(methods, function(x) {
    startsWith(x, "use") && endsWith(x, "Wrapper")
  })
  return(methods[condition])
}

#' @title Get list of all plots useable with the data table
#'
#' @description
#' Get a complete list of all plots implemented within the benchmarkVis application which are combinable with the input data table.
#' (depends on the presence/absence of measures, iteration and list data)
#'
#' @param dt input data table
#' @return Return a list of plots suitable for the given data
#' @export
getValidPlots = function(dt) {
  all.plots = listPlots() #all plots available in the package
  valid.plots = list()
  if (getMeasuresCount(dt) > 0) {
    measures.plots = sapply(all.plots, function(plot) {!grepl("Iteration", plot) && !grepl("List", plot)})
    valid.plots = c(all.plots[measures.plots])
  }
  if (getIterationAlgorithmsCount(dt) > 0) {
    iteration.plots = sapply(all.plots, function(plot) {grepl("Iteration", plot)})
    valid.plots = c(valid.plots, all.plots[iteration.plots])
  }
  if (getListsCount(dt) > 0) {
    list.plots = sapply(all.plots, function(plot) {grepl("List", plot)})
    valid.plots = c(valid.plots, all.plots[list.plots])
  }
  return(valid.plots)
}

