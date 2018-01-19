context("Mlr Benchmark Wrapper")

#  ===================== Basic Setup =====================
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
df = useMlrBenchmarkWrapper(bmr)
#  =======================================================

# Check if wrapped mlr benchmak object equals mlr.benchmark.example
test_that("MlrBenchmarkWrapper Test", {
  # Check if columns are in dataframe
  expect_true("timetrain.test.mean" %in% colnames(df) &&
      is.numeric(df$timetrain.test.mean))
  expect_true("replication.timetrain" %in% colnames(df) &&
      is.vector(df$replication.timetrain))
  # Remove checked columns (can not compare execution time)
  df = subset(df, select = -c(timetrain.test.mean, replication.timetrain))
  df2 = subset(mlr.benchmark.example,
    select = -c(timetrain.test.mean, replication.timetrain))
  # Identical?
  expect_identical(df, df2)
})

# Check dataframe structure
test_that("MlrBenchmarkWrapper Structure", {
  # Check structure
  expect_true(checkStructure(df))
})
