# Startup message
.onAttach = function(libname, pkgname) {
  packageStartupMessage("Welcome to the benchmarkVis package. For instructions visit the wiki at https://github.com/collinleiber/benchmarkVis/wiki")
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
  condition = sapply(methods, function(x) {startsWith(x, "create") && endsWith(x, "Plot")})
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
  condition = sapply(methods, function(x) {startsWith(x, "use") && endsWith(x, "Wrapper")})
  return(methods[condition])
}
