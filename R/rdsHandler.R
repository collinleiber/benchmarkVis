#' @title Import an .rds file as a benchmarkVis data table
#'
#' @description
#' Import a matching .rds file as a benchmarkVis compatible data table.
#' Check the file "extdata/ml.benchmark.rds" to see an example of the correct file structure
#'
#' @param input.rds path to the input .rds file
#' @return a data table with the benchmarkVis specific structure
#' @export
rdsImport = function(input.rds) {
  checkmate::assert_true(endsWith(input.rds, ".rds"))
  # Load rds file
  rds = readRDS(input.rds)
  # Create data table
  dt = data.table::data.table(rds, stringsAsFactors = TRUE)
  # Check structure
  checkmate::assert_true(checkStructure(dt))
  # Return created data table
  return(dt)
}

#' @title Export a benchmarkVis data table as .rds file
#'
#' @description
#' Export the specified benchmarkVis compatible data table as an .rds file.
#'
#' @param dt the benchmarkVis data table
#' @param file.path path to save the file to
#' @export
#' @examples
#' rdsExport(mlr.benchmark.example, "test.rds")
rdsExport = function(dt, file.path) {
  checkmate::assert_true(endsWith(file.path, ".rds"))
  checkmate::assert_true(checkStructure(dt))
  # Write to .rds file
  rds = saveRDS(dt, file.path)
}
