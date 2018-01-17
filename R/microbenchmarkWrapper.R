#' @title Insert microbenchmark object into benchmarkVis application
#'
#' @description
#' Create a dataframe useable within the benchmarkVis application out of an microbenchmark object.
#' All important imformation will be exluded from the input object and transformed into a appropriate dataframe 
#'
#' @param benchmark a microbenchmark object
#' @return a dataframe with the benchmarkVis specific structure
#' @export
#' @examples
#' library(microbenchmark)
#' x = runif(100)
#' benchmark = microbenchmark(
#' sqrt(x),
#' x ^ 0.5
#' )
#' df = useMicrobenchmarkWrapper(benchmark)
useMicrobenchmarkWrapper = function(benchmark) {
  # Create summary object. Holds better accessible information
  summary = summary(benchmark)
  # Create dataframe
  df = data.frame(
    problem = "unknown",
    algorithm = summary$expr,
    replication = "repitition",
    min = summary$min,
    lq = summary$lq,
    mean = summary$mean,
    median = summary$median,
    uq = summary$uq,
    max = summary$max
  )
  # Add lists to the dataframe (not possible within constructor)
  df$problem.parameter = repList(list(), nrow(summary))
  df$algorithm.parameter = repList(list(unit = attr(summary, "unit")), nrow(summary))
  df$replication.parameter = repList(list(times = summary$neval[[1]]), nrow(summary))
  # Change order
  df = df[, c(1, 10, 2, 11, 3, 12, 4:9)]
  # Get the replication values out of the benchmark dataframe
  replication.time = list()
  for (i in seq(summary$expr)) {
    replication.time[[i]] = benchmark[benchmark$expr == benchmark$expr[i], ]$time
  }
  df$replication.values = replication.time
  # Return created dataframe
  return(df)
}

#' @title Insert mlr benchmark RDS file into benchmarkVis application
#'
#' @description
#' Load the specified file and pass it on the the useMicrobenchmarkWrapper function.
#' Create a dataframe useable within the benchmarkVis application out of an microbenchmark object.
#' All important imformation will be exluded from the input object and transformed into a appropriate dataframe
#'
#' @param input.file Path to the input microbenchmark RDS file
#' @return a dataframe with the benchmarkVis specific structure
#' @export
useMicrobenchmarkFileWrapper = function(input.file) {
  benchmark = readRDS(input.file)
  return(useMicrobenchmarkWrapper(benchmark))
}
