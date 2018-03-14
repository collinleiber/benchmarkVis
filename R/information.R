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

#' @title Get list of all possible plots in a readable format: "Type: Name"
#'
#' @description
#' Get a complete list of all plots implemented within the benchmarkVis application. Uses the listPlots() method and transforms the result into the better readable format: "Type: Name"
#'
#' @param input.list the plot list you want to beautify
#' @return vector containing all plots in a readable format
#' @export
getPrettyPlotList = function(input.list) {
  pretty.list = sapply(input.list, function(x) {
    switch(
      x,
      "createBarPlot" = "Measure: Bar Plot",
      "createBoxPlot" = "Measure: Box Plot",
      "createIterationDualParameterPlot" = "Iteration: Dual Parameter Plot",
      "createIterationParameterPlot" = "Iteration: Parameter Plot",
      "createIterationLinePlot" = "Iteration: Line Plot",
      "createListDensityPlot" = "List: Density Plot",
      "createListDensityRankPlot" = "List: Density Rank Plot",
      "createListLinePlot" = "List: Line Plot",
      "createListRankMatrixBarPlot" = "List: Rank Matrix Bar Plot",
      "createParallelCoordinatesPlot" = "Measure: Parallel Coordinates Plot",
      "createRadarPlot" = "Measure: Radar Plot",
      "createRankMatrixBarPlot" = "Measure: Rank Matrix Bar Plot",
      "createRankPlot" = "Measure: Rank Plot",
      "createScatterPlot" = "Measure: Scatter Plot",
      "createSummaryPlot" = "Measure: Summary Plot",
      x
    )
  })
  return(pretty.list)
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

