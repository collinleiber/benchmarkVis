#' @title Check the columns of a benchmarkVis compatible data table
#'
#' @description
#' Check if data table just contains legit columns.
#' Should be the six standard columns (problem, problem.parameter, algorithm, ...) + measures + lists
#'
#' @param dt data table to check
#' @return True if test was successful
#' @export
#' @examples
#' checkColumnNames(microbenchmark.example)
checkColumnNames = function(dt) {
  checkmate::assert_true(getMainColumnsCount(dt) + getParameterColumnsCount(dt) +
      getMeasuresCount(dt) + getListsCount(dt) == ncol(dt))
  checkmate::assert_true("problem" %in% colnames(dt))
  checkmate::assert_true("algorithm" %in% colnames(dt))
  return(TRUE)
}

#' @title Check the structure of a benchmarkVis compatible data table
#'
#' @description
#' Check if the structure of a data table satisfies the benchmarkVis requirements.
#' If it does the method will return TRUE else an error will be thrown
#'
#' @param dt data table to check
#' @return True if test was successful
#' @export
#' @examples
#' checkStructure(microbenchmark.example)
checkStructure = function(dt) {
  # Check column count (should be > 6)
  checkmate::assert_data_table(dt,
    any.missing = TRUE,
    min.rows = 1,
    min.cols = 3)
  # Check measure count + list count (must be >1)
  checkmate::assert_true(getMeasuresCount(dt) + getListsCount(dt) > 0)
  # Check if data table contains not allowed columns
  checkmate::assert_true(checkColumnNames(dt))
  # Check basic structure
  for (x in getMainColumns(dt)) {
    checkmate::assert_false(anyNA(dt[[x]]))
    checkmate::assert_true(is.factor(dt[[x]]))
  }
  for (x in getParameterColumns(dt)) {
    checkmate::assert_false(anyNA(dt[[x]]))
    checkmate::assert_true(is.list(dt[[x]]))
    checkmate::assert_true(all(sapply(dt[[x]], is.list)))
  }
  # Check measures and list measures
  for (x in getMeasures(dt)) {
    checkmate::assert_true(is.numeric(dt[[x]]))
  }
  for (x in getLists(dt)) {
    checkmate::assert_true(is.vector(dt[[x]]))
    checkmate::assert_true(all(sapply(dt[[x]], is.numeric)))
  }
  # Check iteration algorithms. Iteration parameter must be included
  for (x in getIterationAlgorithms(dt)) {
    checkmate::assert_true(all(sapply(dt[dt$algorithm == x, ]$algorithm.parameter, function(x) {
      "iteration" %in% names(x)
    })))
    # Check for duplicate iteration values
    checkmate::assert_true(0 == anyDuplicated(sapply(dt[dt$algorithm == x, ]$algorithm.parameter, function(x) {
      x$iteration
    })))
  }
  # All checks passed. Return true
  return(TRUE)
}
