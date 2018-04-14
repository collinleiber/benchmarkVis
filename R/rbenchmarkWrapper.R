#' @title Insert rbenchmark object into benchmarkVis application
#'
#' @description
#' Create a data table useable with the benchmarkVis application out of an rbenchmark object.
#' All important imformation will be exluded from the input object and transformed into a appropriate data table
#'
#' @param benchmark a rbenchmark object
#' @return a data table with the benchmarkVis specific structure
#' @export
#' @examples
#' library(rbenchmark)
#' x = runif(1000)
#' benchmark = rbenchmark::benchmark(
#'  shell_sort = sort(x, method = "shell"),
#'  quick_sort = sort(x, method = "quick"),
#'  radix_sort = sort(x, method = "radix"),
#'  columns = c(
#'    "test", "replications", "elapsed"),
#'  order = "elapsed",
#'  replications = c(100, 20)
#')
#' dt = useRbenchmarkWrapper(benchmark)
useRbenchmarkWrapper = function(benchmark) {
  # Create data table
  dt = data.table::data.table(
    problem = "unknown",
    algorithm = as.character(benchmark$test),
    replication = "repetition",
    replication.parameter = lapply(benchmark$replications, function(x) {
      list(iters = x)
    })
  )
  # Check the possible measures
  measures = c("elapsed",
    "relative",
    "user.self",
    "sys.self",
    "user.child",
    "sys.child")
  for (x in measures) {
    if (x %in% colnames(benchmark)) {
      dt[[paste("measure", x, sep = ".")]] = as.numeric(benchmark[[x]])
    }
  }
  # Check structure
  checkmate::assert_true(checkStructure(dt))
  # Return created data table
  return(dt)
}

#' @title Insert rbenchmark RDS file into benchmarkVis application
#'
#' @description
#' Load the specified file and pass it on the the useRbenchmarkWrapper function.
#' Create a data table useable within the benchmarkVis application out of an rbenchmark object.
#' All important imformation will be exluded from the input object and transformed into a appropriate data table
#'
#' @param input.file Path to the input rbenchmark RDS file
#' @return a data table with the benchmarkVis specific structure
#' @export
useRbenchmarkFileWrapper = function(input.file) {
  benchmark = readRDS(input.file)
  return(useRbenchmarkWrapper(benchmark))
}
