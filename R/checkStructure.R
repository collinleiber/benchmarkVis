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
  checkmate::assert_true(6 + getMeasuresCount(dt) + getListsCount(dt) == ncol(dt))
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
    min.cols = 7)
  # Check if data table contains not allowed columns
 checkmate::assert_true(checkColumnNames(dt))
  # Check basic structure
  checkmate::assert_true(
    "problem" %in% colnames(dt) &&
      is.factor(dt$problem)
  )
  checkmate::assert_true(
    "problem.parameter" %in% colnames(dt) &&
      is.list(dt$problem.parameter)
  )
  checkmate::assert_true(
    "algorithm" %in% colnames(dt) &&
      is.factor(dt$algorithm)
  )
  checkmate::assert_true(
    "algorithm.parameter" %in% colnames(dt) &&
      is.list(dt$algorithm.parameter)
  )
  checkmate::assert_true(
    "replication" %in% colnames(dt) &&
      is.factor(dt$replication)
  )
  checkmate::assert_true(
    "replication.parameter" %in% colnames(dt) &&
      is.list(dt$replication.parameter)
  )
  # Check measure count + list count (must be >1)
  checkmate::assert_true(getMeasuresCount(dt) + getListsCount(dt) > 0)
  # Check measures and list measures
  for (x in colnames(dt)) {
    if (startsWith(x, "list.")) {
      # Should be a list measure vector
      checkmate::assert_true(is.vector(dt[[x]]))
    } else if (startsWith(x, "measure.")) {
      # Should be a measure
      checkmate::assert_true(is.numeric(dt[[x]]))
    }
  }
  # Check for replication iters parameter
  for (i in nrow(dt)) {
    # Every replication != null needs iters parameter
    if (dt$replication[[i]] != "unknown") {
      checkmate::assert_true(
        "iters" %in% names(dt$replication.parameter[[i]]) &&
          is.numeric(dt$replication.parameter[[i]]$iters)
      )
    }
  }
  # All checks passed. Return true
  return(TRUE)
}
