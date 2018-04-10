# Startup message
.onAttach = function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to the benchmarkVis package. For instructions visit the wiki at https://github.com/collinleiber/benchmarkVis/wiki"
  )
}

#' @title Get all measures
#' @description
#' Get the names of measure columns within the data table.
#' Measures columns start with "measure."
#'
#' @param dt input data table
#' @return vector containing all measure names
#' @export
#' @examples
#' getMeasures(mlr.benchmark.example)
getMeasures = function(dt) {
  return(subset(names(dt), startsWith(names(dt), "measure.")))
}

#' @title Get all lists
#' @description
#' Get the names of list columns within the data table.
#' List columns start with "list."
#'
#' @param dt input data table
#' @return vector containing all list names
#' @export
#' @examples
#' getLists(mlr.benchmark.example)
getLists = function(dt) {
  return(subset(names(dt), startsWith(names(dt), "list.")))
}

#' @title Get all iteration algorithms
#' @description
#' Get the names of algorithms with multiple iterations within the data table.
#' Algorithms for which the parameters contain "iteration" field.
#'
#' @param dt input data table
#' @return vector containing all iteration algorithm names
#' @export
#' @examples
#' getIterationAlgorithms(mlr.tuning.example)
getIterationAlgorithms = function(dt) {
  iteration.algorithms = vector()
  for (row in nrow(dt)) {
    if ("iteration" %in% names(dt[[row, "algorithm.parameter"]]) &&
        !as.character(dt[[row, "algorithm"]]) %in% iteration.algorithms) {
      iteration.algorithms = c(iteration.algorithms, as.character(dt[[row, "algorithm"]]))
    }
  }
  return(iteration.algorithms)
}

#' @title Get all main columns
#' @description
#' Get the names of main columns within the data table.
#' Can be "problem", "algorithm", "replication". "Problem" and "Algorithm" are mandatory.
#'
#' @param dt input data table
#' @return vector containing all main column names
#' @export
#' @examples
#' getMainColumns(mlr.benchmark.example)
getMainColumns = function(dt) {
  main.columns = c("problem", "algorithm", "replication")
  return(intersect(names(dt), main.columns))
}

#' @title Get all parameter columns
#' @description
#' Get the names of parameter columns within the data table.
#' Can be "problem.parameter", "algorithm.parameter", "replication.parameter".
#'
#' @param dt input data table
#' @return vector containing all parameter column names
#' @export
#' @examples
#' getParameterColumns(mlr.benchmark.example)
getParameterColumns = function(dt) {
  main.columns = c("problem.parameter", "algorithm.parameter", "replication.parameter")
  return(intersect(names(dt), main.columns))
}

#' @title Get list of all possible plots
#'
#' @description
#' Get a complete list of all plots implemented within the benchmarkVis application
#'
#' @return vector containing all possbile plots
#' @export
#' @examples
#' listPlots()
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
#' @examples
#' listWrappers()
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
#' @examples
#' getValidPlots(mlr.benchmark.example)
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

