context("Do Aggregation")

test_that("aggregation function is valid", {
  test.fun.1 = function(x) 1
  test.fun.2 = function(x) x #if x is a list then returns a list
  test.fun.3 = function(x) "test"
  aggfun.list = list(max, min, mean, sd, median, IQR, var, test.fun.1)
  no.aggfun.list = list(rank, quantile, range, test.fun.2, test.fun.3)
  for (fun in aggfun.list){
    expect_true(check.agg.valid(fun))
  }
  for (fun in no.aggfun.list){
    expect_false(check.agg.valid(fun))
  }
})

# Check if Do aggregation for mlr benchmark with mean as aggfun works correctly
test_that("Do aggregation for mlr benchmark", {
  agd = get.result(
    groupby = c("problem", "algorithm"),
    aggfun = "mean",
    aggcol = c("measure.mmce.test.mean", "measure.ber.test.mean"),
    df = mlr.benchmark.example
  )
  expect_true(is.data.frame(agd) && ncol(agd) == 4L)
  expect_true(is.numeric(agd[, 3L]) && is.numeric(agd[, 4L]))
  expect_equal(
    colnames(agd),
    c(
      "problem",
      "algorithm",
      "mean_measure.mmce.test.mean",
      "mean_measure.ber.test.mean"
    )
  )
})

# Check if Do aggregation for microbenchmark with mean as aggfun works correctly
test_that("Do aggregation for microbenchmark", {
  agd = get.result(
    groupby = c("problem", "algorithm"),
    aggfun = "mean",
    aggcol = c("measure.mean", "measure.median"),
    df = microbenchmark.example
  )
  expect_true(is.data.frame(agd) && ncol(agd) == 4L)
  expect_true(is.numeric(agd[, 3L]) && is.numeric(agd[, 4L]))
  expect_equal(
    colnames(agd),
    c(
      "problem",
      "algorithm",
      "mean_measure.mean",
      "mean_measure.median"
    )
  )
})

# Check if Do aggregation for microbenchmark with user-defined aggfun works correctly
test_that("Do aggregation for microbenchmark", {
  agd = get.result(
    groupby = c("problem", "algorithm"),
    aggfun = "function(x) 1",
    aggcol = c("measure.mean", "measure.median"),
    df = microbenchmark.example
  )
  expect_true(is.data.frame(agd) && ncol(agd) == 4L)
  expect_true(is.numeric(agd[, 3L]) && is.numeric(agd[, 4L]))
  expect_equal(
    colnames(agd),
    c(
      "problem",
      "algorithm",
      "function(x) 1_measure.mean",
      "function(x) 1_measure.median"
    )
  )
})
