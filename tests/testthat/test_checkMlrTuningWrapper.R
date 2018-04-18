context("Mlr Tuning Wrapper")

# Check if wrapped mlr tuning object equals mlr.tuning.example
test_that("MlrTuningWrapper Test", {
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
  dt = useMlrTuningWrapper(res)
  # Second tune control strategy
  ctrl = mlr::makeTuneControlGrid(resolution = 15L)
  res = mlr::tuneParams(
    "classif.ksvm",
    task = mlr::iris.task,
    resampling = rdesc,
    par.set = num.ps,
    control = ctrl,
    measures = list(mlr::acc, mlr::setAggregation(mlr::acc, test.sd)),
    show.info = FALSE
  )
  dt = rbind(dt, useMlrTuningWrapper(res))
  # Identical?
  expect_equal(dt, mlr.tuning.example)
})
