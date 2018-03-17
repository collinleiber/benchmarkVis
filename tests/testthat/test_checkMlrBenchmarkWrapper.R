context("Mlr Benchmark Wrapper")

# Check if wrapped mlr benchmak object equals mlr.benchmark.example
test_that("MlrBenchmarkWrapper Test", {
  set.seed(2017)
  # Create mlr benchmark
  lrns = list(
    mlr::makeLearner("classif.lda", id = "lda"),
    mlr::makeLearner("classif.rpart", id = "rpart"),
    mlr::makeLearner("classif.randomForest", id = "randomForest")
  )
  ring.task = mlr::convertMLBenchObjToTask("mlbench.ringnorm", n = 600)
  wave.task = mlr::convertMLBenchObjToTask("mlbench.waveform", n = 600)
  tasks = list(mlr::iris.task, mlr::sonar.task, mlr::pid.task, ring.task, wave.task)
  rdesc = mlr::makeResampleDesc("CV", iters = 10)
  meas = list(mlr::mmce, mlr::ber, mlr::timetrain)
  bmr = mlr::benchmark(lrns, tasks, rdesc, meas, show.info = FALSE)
  dt = useMlrBenchmarkWrapper(bmr)
  # Check if columns are in data table
  expect_true("measure.timetrain.test.mean" %in% colnames(dt) &&
      is.numeric(dt$measure.timetrain.test.mean))
  expect_true("list.timetrain" %in% colnames(dt) &&
      is.vector(dt$list.timetrain))
  # Remove checked columns (can not compare execution time)
  dt = subset(dt, select = -c(measure.timetrain.test.mean, list.timetrain))
  dt2 = subset(mlr.benchmark.example,
    select = -c(measure.timetrain.test.mean, list.timetrain))
  # Identical?
  expect_identical(dt, dt2)
})
