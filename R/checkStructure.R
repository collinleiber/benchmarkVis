#' @title Check the structure of a benchmarkVis compatible dataframe
#'
#' @description
#' Check if the structure of a dataframe satisfies the benchmarkVis requirements.
#' If it does the method will return TRUE else an error will be thrown
#'
#' @param df dataframe to check
#' @return True if test was successful
#' @export
#' @examples
#' checkStructure(microbenchmark.example)
checkStructure = function(df) {
  # Check column count (should be > 6)
  checkmate::assert_data_frame(df, any.missing = TRUE, min.rows = 1, min.cols = 7)
  # Check basic structure
  checkmate::assert_true("problem" %in% colnames(df) &&
      is.factor(df$problem) && which(colnames(df) == "problem") == 1)
  checkmate::assert_true("problem.parameter" %in% colnames(df) &&
      is.list(df$problem.parameter) && which(colnames(df) == "problem.parameter") == 2)
  checkmate::assert_true("algorithm" %in% colnames(df) &&
      is.factor(df$algorithm) && which(colnames(df) == "algorithm") == 3)
  checkmate::assert_true("algorithm.parameter" %in% colnames(df) &&
      is.list(df$algorithm.parameter) && which(colnames(df) == "algorithm.parameter") == 4)
  checkmate::assert_true("replication" %in% colnames(df) &&
      is.factor(df$replication) && which(colnames(df) == "replication") == 5)
  checkmate::assert_true("replication.parameter" %in% colnames(df) &&
      is.list(df$replication.parameter) && which(colnames(df) == "replication.parameter") == 6)
  # Check measure count (must be >1)
  checkmate::assert_true(getMeasureCount(df) > 0)
  # Check measures and replication measures
  for (x in colnames(df)) {
    if (startsWith(x, "replication.") && x != "replication.parameter") {
      # Should be a replication measure vector
      checkmate::assert_true(is.vector(df[[x]]) && which(colnames(df) == x) > 6 + getMeasureCount(df))
    } else if (!startsWith(x, "replication") &&
        !startsWith(x, "algorithm") && !startsWith(x, "problem")) {
      # Should be a measure
      checkmate::assert_true(is.numeric(df[[x]]) && which(colnames(df) == x) > 6 && which(colnames(df) == x) <= 6 + getMeasureCount(df))
    }
  }
  # Check for replication iters parameter
  for (i in nrow(df)) {
    # Every replication != null needs iters parameter
    if (df$replication[[i]] != "unknown") {
      checkmate::assert_true(
        "iters" %in% names(df$replication.parameter[[i]]) &&
          is.numeric(df$replication.parameter[[i]]$iters)
      )
    }
  }
  # All checks passed. Return true
  return(TRUE)
}
