#' @title Insert mlr tuning object into benchmarkVis application
#'
#' @description
#' Create a dataframe useable within the benchmarkVis application out of an mlr tuning object.
#' All important imformation will be exluded from the input object and transformed into a appropriate dataframe
#'
#' @param res a mlr tuning result object
#' @return a dataframe with the benchmarkVis specific structure
#' @export
#' @examples
#' library(mlr)
#' num.ps = makeParamSet(
#'  makeNumericParam("C", lower = -10, upper = 10, trafo = function(x) 10 ^ x),
#'  makeNumericParam("sigma", lower = -10, upper = 10, trafo = function(x) 10 ^ x)
#' )
#' ctrl = makeTuneControlRandom(maxit = 100L)
#' rdesc = makeResampleDesc("CV", iters = 3L)
#' res = tuneParams("classif.ksvm", task = iris.task, resampling = rdesc, par.set = num.ps, control = ctrl,
#'  measures = list(acc, setAggregation(acc, test.sd)), show.info = FALSE)
#' df = useMlrTuningWrapper(res)
useMlrTuningWrapper = function(res) {
  # Get help parameters
  data = generateHyperParsEffectData(res)
  # Create dataframe
  df = data.frame(
    problem = rep(data$optimization, length(data$data[[1]])),
    algorithm = rep(res$learner$id, length(data$data[[1]])),
    replication = "unknown"
  )
  # Add lists
  df$problem.parameter = repList(res$control$extra.args, length(data$data[[1]]))
  df$algorithm.parameter = lapply(seq(data$data[[1]]), function(x) {
    # Add algorithm parameters (inculding iteration)
    params = list()
    params$iteration = x
    for (ele in names(res$x)) {
      params[[ele]] = data$data[[ele]][[x]]
    }
    return(params)
  })
  df$replication.parameter = repList(list(), length(data$data[[1]]))
  # Change order
  df = df[, c(1, 4, 2, 5, 3, 6)]
  # Add measures
  for (ele in names(res$y)) {
    df[[ele]] = sapply(data$data[[ele]], as.numeric)
  }
  # Return created dataframe
  return(df)
}

#' @title Insert mlr tuning RDS file into benchmarkVis application
#'
#' @description
#' Load the specified file and pass it on the the useMlrTuningWrapper function.
#' Create a dataframe useable within the benchmarkVis application out of an mlr tuning object.
#' All important imformation will be exluded from the input object and transformed into a appropriate dataframe
#'
#' @param input.file Path to the input mlr tuning RDS file
#' @return a dataframe with the benchmarkVis specific structure
#' @export
useMlrTuningFileWrapper = function(input.file) {
  res = readRDS(input.file)
  return(useMlrTuningWrapper(res))
}
