context("Bar Plots")

# Check if creating a Bar Plot throws an error
test_that("createBarPlot for mlr benchmark", {
  p = createBarPlot(mlr.benchmark.example, "measure.ber.test.mean", "algorithm", TRUE)
  expect_true(is.list(p))
  p = createBarPlot(mlr.benchmark.example, "measure.ber.test.mean", "problem", TRUE)
  expect_true(is.list(p))
  p = createBarPlot(mlr.benchmark.example, "measure.ber.test.mean", "replication", TRUE)
  expect_true(is.list(p))
  tmp = mlr.benchmark.example
  tmp$replication = NULL
  p = createBarPlot(tmp, "measure.ber.test.mean", "algorithm", TRUE)
  expect_true(is.list(p))
  p = createBarPlot(tmp, "measure.ber.test.mean", "problem", TRUE, TRUE)
  expect_true(is.list(p))
})

# Check if creating a Bar Plot throws an error
test_that("createBarPlot for microbenchmark", {
  p = createBarPlot(microbenchmark.example, "measure.mean", "problem", FALSE)
  expect_true(is.list(p))
  tmp = microbenchmark.example
  tmp$replication = NULL
  p = createBarPlot(tmp, "measure.mean", "algorithm", FALSE)
  expect_true(is.list(p))
})


# Check if creating a Rank Matrix Bar Plot throws an error
test_that("createRankMatrixBarPlot for mlr benchmark", {
  p = createRankMatrixBarPlot(mlr.benchmark.example, "measure.ber.test.mean", stacked = TRUE)
  expect_true(is.list(p))
})

# Check if creating a Rank Matrix Bar Plot throws an error
test_that("createRankMatrixBarPlot for microbenchmark", {
  p = createRankMatrixBarPlot(microbenchmark.example, stacked = FALSE)
  expect_true(is.list(p))
})
