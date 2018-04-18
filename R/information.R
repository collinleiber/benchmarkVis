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

#' @title Get all tuning sessions
#' @description
#' Get the combination of (problem, algorithm, replicaiton) with multiple iterations within the data table.
#' Should be used to tune the algorithm parameters.
#' Combinations can be recognised by the occurrence of the "iteration" field in the algorithm.parameter column.
#'
#' @param dt input data table
#' @return list containing vectors of form: (problem, algorithm, replicaiton)
#' @export
#' @examples
#' getTunings(mlr.tuning.example)
getTunings = function(dt) {
  tuning.combination = list()
  i = 1
  for (row in seq(nrow(dt))) {
    if ("iteration" %in% names(dt[[row, "algorithm.parameter"]])) {
      if ("replication" %in% getMainColumns(dt)) {
        entry = c(dt[[row, "problem"]], dt[[row, "algorithm"]], dt[[row, "replication"]])
      } else {
        entry = c(dt[[row, "problem"]], dt[[row, "algorithm"]])
      }
      # Check if tuning.combination already contains vector
      if (!any(sapply(tuning.combination, function(x) {
        return(isTRUE(all.equal(x, entry)))
      }))) {
        tuning.combination[[i]] = entry
        i = i + 1
      }
    }
  }
  return(tuning.combination)
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
#' (depends on the presence/absence of measures, parameters, tunings and list data)
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
    measures.plots = sapply(all.plots, function(plot) {!grepl("List", plot) && !grepl("Parameter", plot) && !grepl("Tuning", plot)})
    valid.plots = c(all.plots[measures.plots])
  }
  if (getListsCount(dt) > 0) {
    list.plots = sapply(all.plots, function(plot) {grepl("List", plot)})
    valid.plots = c(valid.plots, all.plots[list.plots])
  }
  if (getParameterColumnsCount(dt) > 0) {
    parameter.plots = sapply(all.plots, function(plot) {grepl("Parameter", plot)})
    valid.plots = c(valid.plots, all.plots[parameter.plots])
  }
  if (getTuningsCount(dt) > 0) {
    iteration.plots = sapply(all.plots, function(plot) {grepl("Tuning", plot)})
    valid.plots = c(valid.plots, all.plots[iteration.plots])
  }
  return(valid.plots)
}

