makeMLRBenchmarkWrapper = function(input.file) {
  bmr = readRDS(input.file)
  # Add problem column
  learner.count = length(bmr$learners)
  tasks.count = length(bmr$result)
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

}

input = "data/mlrBenchmark.rds"
makeMLRBenchmarkWrapper(input)
