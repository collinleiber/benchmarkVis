context("Aggregation")

test_that("aggregation function is valid", {
  test.fun.1 = function(x) 1
  test.fun.2 = function(x) x #if x is a list then returns a list
  test.fun.3 = function(x) "test"
  aggfun.list = list(max, min, mean, sd, median, IQR, var, test.fun.1)
  no.aggfun.list = list(rank, quantile, range, test.fun.2, test.fun.3)
  for (fun in aggfun.list){
    expect_true(check.aggregation.valid(fun))
  }
  for (fun in no.aggfun.list){
    expect_false(check.aggregation.valid(fun))
  }
})

# Check if Do aggregation for mlr benchmark with mean as aggfun works correctly
test_that("Do aggregation for mlr benchmark", {
  agd = aggregation.apply(
    groupby = c("problem", "algorithm"),
    aggfun = "mean",
    aggcol = c("measure.mmce.test.mean", "measure.ber.test.mean"),
    dt = mlr.benchmark.example
  )
  expect_true(is.data.frame(agd) && ncol(agd) == 4L)
  expect_true(is.numeric(agd[, 3L]) && is.numeric(agd[, 4L]))
  expect_equal(
    colnames(agd),
    c(
      "problem",
      "algorithm",
      "measure.mmce.test.mean_mean",
      "measure.ber.test.mean_mean"
    )
  )
})

# Check if Do aggregation for microbenchmark with mean as aggfun works correctly
test_that("Do aggregation for microbenchmark", {
  agd = aggregation.apply(
    groupby = c("problem", "algorithm"),
    aggfun = "mean",
    aggcol = c("measure.mean", "measure.median"),
    dt = microbenchmark.example
  )
  expect_true(is.data.frame(agd) && ncol(agd) == 4L)
  expect_true(is.numeric(agd[, 3L]) && is.numeric(agd[, 4L]))
  expect_equal(
    colnames(agd),
    c(
      "problem",
      "algorithm",
      "measure.mean_mean",
      "measure.median_mean"
    )
  )
})

# Check if Do aggregation for microbenchmark with user-defined aggfun works correctly
test_that("Do aggregation for microbenchmark", {
  agd = aggregation.apply(
    groupby = c("problem", "algorithm"),
    aggfun = "function(x) 1",
    aggcol = c("measure.mean", "measure.median"),
    dt = microbenchmark.example
  )
  expect_true(is.data.frame(agd) && ncol(agd) == 4L)
  expect_true(is.numeric(agd[, 3L]) && is.numeric(agd[, 4L]))
  expect_equal(
    colnames(agd),
    c(
      "problem",
      "algorithm",
      "measure.mean_function(x) 1",
      "measure.median_function(x) 1"
    )
  )
})

# Check if aggregation for microbenchmark with a non valid aggregation function works correctly
test_that("Do aggregation for microbenchmark with a non valid aggregation function", {
  result = aggregation.apply(
    groupby = c("problem", "algorithm"),
    aggfun = "log2",
    aggcol = "measure.mean",
    dt = microbenchmark.example
  )
  expect_equal(result, microbenchmark.example)
})
