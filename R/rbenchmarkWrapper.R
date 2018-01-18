#' @title Insert rbenchmark object into benchmarkVis application
#'
#' @description
#' Create a dataframe useable within the benchmarkVis application out of an rbenchmark object.
#' All important imformation will be exluded from the input object and transformed into a appropriate dataframe
#'
#' @param benchmark a rbenchmark object
#' @return a dataframe with the benchmarkVis specific structure
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
#' df = useRbenchmarkWrapper(benchmark)
useRbenchmarkWrapper = function(benchmark) {
  # Create dataframe
  df = data.frame(
    problem = "unknown",
    algorithm = benchmark$test,
    replication = "repitition"
  )
  # Add lists to the dataframe (not possible within constructor)
  df$problem.parameter = repList(list(), nrow(benchmark))
  df$algorithm.parameter = repList(list(), nrow(benchmark))
  df$replication.parameter = lapply(benchmark$replications, function(x) {
    list(iters = x)
  })
  # Change order
  df = df[, c(1, 4, 2, 5, 3, 6)]
  # Check the possible measures
  measures = c("elapsed",
    "relative",
    "user.self",
    "sys.self",
    "user.child",
    "sys.child")
  for (x in measures) {
    if (x %in% colnames(benchmark)) {
      df[[x]] = as.numeric(benchmark[[x]])
    }
  }
  # Return created dataframe
  return(df)
}

#' @title Insert rbenchmark RDS file into benchmarkVis application
#'
#' @description
#' Load the specified file and pass it on the the useRbenchmarkWrapper function.
#' Create a dataframe useable within the benchmarkVis application out of an rbenchmark object.
#' All important imformation will be exluded from the input object and transformed into a appropriate dataframe
#'
#' @param input.file Path to the input rbenchmark RDS file
#' @return a dataframe with the benchmarkVis specific structure
#' @export
useRbenchmarkFileWrapper = function(input.file) {
  benchmark = readRDS(input.file)
  return(useRbenchmarkWrapper(benchmark))
}
