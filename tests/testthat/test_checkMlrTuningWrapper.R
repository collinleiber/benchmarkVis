context("Mlr Tuning Wrapper")

#  ===================== Basic Setup =====================
set.seed(2017)
# Create mlr benchmark
num.ps = ParamHelpers::makeParamSet(
  ParamHelpers::makeNumericParam(
    "C",
    lower = -10,
    upper = 10,
    trafo = function(x)
      10 ^ x
  ),
  ParamHelpers::makeNumericParam(
    "sigma",
    lower = -10,
    upper = 10,
    trafo = function(x)
      10 ^ x
  )
)
ctrl = mlr::makeTuneControlRandom(maxit = 100L)
rdesc = mlr::makeResampleDesc("CV", iters = 3L)
res = mlr::tuneParams(
  "classif.ksvm",
  task = mlr::iris.task,
  resampling = rdesc,
  par.set = num.ps,
  control = ctrl,
  measures = list(mlr::acc, mlr::setAggregation(mlr::acc, test.sd)),
  show.info = FALSE
)
df = useMlrTuningWrapper(res)
#  =======================================================

# Check if wrapped mlr tuning object equals mlr.tuning.example
test_that("MlrTuningWrapper Test", {
  # Identical?
  expect_identical(df, mlr.tuning.example)
})

# Check dataframe structure
test_that("MlrBenchmarkWrapper Structure", {
  # Check structure
  expect_true(checkStructure(df))
})
