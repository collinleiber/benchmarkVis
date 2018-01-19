library(mlr)

context("Mlr Benchmark Wrapper")

#  ===================== Basic Setup =====================
set.seed(2017)
# Create mlr benchmark
lrns = list(
  makeLearner("classif.lda", id = "lda"),
  makeLearner("classif.rpart", id = "rpart"),
  makeLearner("classif.randomForest", id = "randomForest")
)
ring.task = convertMLBenchObjToTask("mlbench.ringnorm", n = 600)
wave.task = convertMLBenchObjToTask("mlbench.waveform", n = 600)
tasks = list(iris.task, sonar.task, pid.task, ring.task, wave.task)
rdesc = makeResampleDesc("CV", iters = 10)
meas = list(mmce, ber, timetrain)
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
