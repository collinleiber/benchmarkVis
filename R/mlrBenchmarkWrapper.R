#' @title Insert mlr benchmark object into benchmarkVis application
#'
#' @description
#' Create a dataframe useable within the benchmarkVis application out of an mlr benchmark object.
#' All importont imformation will be read out of the input file and transformed in a matching build dataframe
#'
#' @param bmr a mlr benchmark result object
#' @return a dataframe with the benchmarkVis specific structure
#' @export
#' @examples
#' data("mlr.benchmark")
#' df = useMlrBenchmarkWrapper(mlr.benchmark)
useMlrBenchmarkWrapper = function(bmr) {
  # General variables
  learner.count = length(bmr$learners)
  tasks.count = length(bmr$result)
  # Add problem column
  tasks = rep(names(bmr$result), rep(learner.count, tasks.count))
  df = data.frame(problem = tasks)
  # Add problem parameters column
  task.params = list()
  for (i in 1:tasks.count) {
    params = list()
    params$target = bmr$results[[i]][[1]]$task.desc$target
    params$size = bmr$results[[i]][[1]]$task.desc$size
    task.params[[i]] = params
  }
  task.parameter = rep(task.params, rep(learner.count, tasks.count))
  df$problem.parameter = task.parameter
  # Add algorithm column
  learners = rep(names(bmr$learners), tasks.count)
  df$algorithm = learners
  # Add algorithm parameters
  learner.params = list()
  for (i in 1:learner.count) {
    learner.params[[i]] = bmr$learners[[i]]$par.vals
  }
  learner.parameter = rep(learner.params, tasks.count)
  df$algorithm.parameter = learner.parameter
  # Add replication (each task can have its own replication method)
  replications = list()
  for (i in 1:tasks.count) {
    replications[[i]] = bmr$results[[i]][[1]]$pred$instance$desc$id
  }
  replications = rep(replications, rep(learner.count, tasks.count))
  df$replication = replications
  # Add replications parameters
  replication.params = list()
  for (i in 1:tasks.count) {
    params = list()
    params$iters = bmr$results[[i]][[1]]$pred$instance$desc$iters
    replication.params[[i]] = params
  }
  replication.parameter = rep(replication.params, rep(learner.count, tasks.count))
  df$replication.parameter = replication.parameter
  # Add measures and replication results
  replication.results = list()
  for (measure.nr in seq(bmr$measures)) {
    measure.list = list()
    replication.list = list()
    for (i in 1:tasks.count) {
      for (j in 1:learner.count) {
        measure.list[[(i - 1) * learner.count + j]] = bmr$results[[i]][[j]]$aggr[[measure.nr]]
        replication.list[[(i - 1) * learner.count + j]] = bmr$results[[i]][[j]]$measures.test[[measure.nr +
            1]]
      }
    }
    df[[names(bmr$results[[1]][[1]]$aggr)[[measure.nr]]]] = measure.list
    replication.results[[measure.nr]] = replication.list
  }
  for (i in seq(replication.results)) {
    df[[paste("replication", names(bmr$results[[1]][[1]]$measures.test)[[i +
        1]], sep = ".")]] = replication.results[[i]]
  }
  return(df)
}

#' @title Insert mlr benchmark RDS file into benchmarkVis application
#'
#' @description
#' Load the specified file and path it on the the useMlrBenchmarkWrapper function.
#' Create a dataframe useable within the benchmarkVis application out of an mlr benchmark object.
#' All importont imformation will be read out of the input file and transformed in a matching build dataframe
#'
#' @param input.file Path to the input mlr benchmark RDS file
#' @return a dataframe with the benchmarkVis specific structure
#' @export
useMlrBenchmarkWrapperWithRdsFile = function(input.file) {
  mlr.bmr = readRDS(input.file)
  return(useMlrBenchmarkWrapper(mlr.bmr))
}
