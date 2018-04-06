#' @title Insert microbenchmark object into benchmarkVis application
#'
#' @description
#' Create a data table useable with the benchmarkVis application out of an microbenchmark object.
#' All important imformation will be exluded from the input object and transformed into a appropriate data table
#'
#' @param benchmark a microbenchmark object
#' @return a data table with the benchmarkVis specific structure
#' @export
#' @examples
#' library(microbenchmark)
#' x = runif(100)
#' benchmark = microbenchmark(
#' sqrt(x),
#' x ^ 0.5
#' )
#' dt = useMicrobenchmarkWrapper(benchmark)
useMicrobenchmarkWrapper = function(benchmark) {
  # Create summary object. Holds better accessible information
  summary = summary(benchmark)
  # Create data table
  dt = data.table::data.table(
    problem = "unknown",
    algorithm = summary$expr,
    algorithm.parameter = repList(list(unit = attr(summary, "unit")), nrow(summary)),
    replication = "repitition",
    replication.parameter = repList(list(iters = summary$neval[[1]]), nrow(summary)),
    measure.min = summary$min,
    measure.lq = summary$lq,
    measure.mean = summary$mean,
    measure.median = summary$median,
    measure.uq = summary$uq,
    measure.max = summary$max,
    # Get the replication values out of the benchmark result
    list.values = lapply(summary$expr, function(x) benchmark[benchmark$expr == x, ]$time),
    stringsAsFactors = TRUE
  )
  # Check structure
  checkmate::assert_true(checkStructure(dt))
  # Return created data table
  return(dt)
}

#' @title Insert mlr benchmark RDS file into benchmarkVis application
#'
#' @description
#' Load the specified file and pass it on to the useMicrobenchmarkWrapper function.
#' Create a data table useable with the benchmarkVis application out of an microbenchmark object.
#' All important imformation will be exluded from the input object and transformed into a appropriate data table
#'
#' @param input.file Path to the input microbenchmark RDS file
#' @return a data table with the benchmarkVis specific structure
#' @export
useMicrobenchmarkFileWrapper = function(input.file) {
  benchmark = readRDS(input.file)
  return(useMicrobenchmarkWrapper(benchmark))
}
