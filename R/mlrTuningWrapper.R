#' @title Insert mlr tuning object into benchmarkVis application
#'
#' @description
#' Create a data table useable with the benchmarkVis application out of an mlr tuning object.
#' All important imformation will be exluded from the input object and transformed into a appropriate data table
#'
#' @param res a mlr tuning result object
#' @return a data table with the benchmarkVis specific structure
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
#' dt = useMlrTuningWrapper(res)
useMlrTuningWrapper = function(res) {
  # Get help parameters
  data = generateHyperParsEffectData(res)
  # Create data table
  dt = data.table::data.table(
    problem = rep(data$optimization, length(data$data[[1]])),
    problem.parameter = repList(res$control$extra.args, length(data$data[[1]])),
    algorithm = rep(res$learner$id, length(data$data[[1]])),
    algorithm.parameter = lapply(seq(data$data[[1]]), function(x) {
      # Add algorithm parameters (inculding iteration)
      params = list()
      params$iteration = x
      for (ele in names(res$x)) {
        params[[ele]] = data$data[[ele]][[x]]
      }
      return(params)
    })
    #replication = "unknown",
    #replication.parameter = repList(list(), length(data$data[[1]]))
  )
  # Add measures
  for (ele in names(res$y)) {
    dt[[paste("measure", ele, sep = ".")]] = sapply(data$data[[ele]], as.numeric)
  }
  # Check structure
  checkmate::assert_true(checkStructure(dt))
  # Return created data.table
  return(dt)
}

#' @title Insert mlr tuning RDS file into benchmarkVis application
#'
#' @description
#' Load the specified file and pass it on the the useMlrTuningWrapper function.
#' Create a data table useable with the benchmarkVis application out of an mlr tuning object.
#' All important imformation will be exluded from the input object and transformed into a appropriate data table
#'
#' @param input.file Path to the input mlr tuning RDS file
#' @return a data table with the benchmarkVis specific structure
#' @export
useMlrTuningFileWrapper = function(input.file) {
  res = readRDS(input.file)
  return(useMlrTuningWrapper(res))
}
