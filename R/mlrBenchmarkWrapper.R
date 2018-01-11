makeMLRBenchmarkWrapper = function(input.file) {
  bmr = readRDS(input.file)
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
  learners = rep(names(bmr$learners),tasks.count)
  df$algorithm = learners
  # Add algorithm parameters

}

input = "data/mlrBenchmark.rds"
makeMLRBenchmarkWrapper(input)
