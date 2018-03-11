context("Bar Plots")

# Check if creating a Bar Plot throws an error
test_that("createBarPlot for mlr benchmark", {
  p = createBarPlot(mlr.benchmark.example, "measure.ber.test.mean", TRUE)
  expect_true(is.list(p))
})

# Check if creating a Bar Plot throws an error
test_that("createBarPlot for microbenchmark", {
  p = createBarPlot(microbenchmark.example, "measure.mean", FALSE)
  expect_true(is.list(p))
})


# Check if creating a Rank Matrix Bar Plot throws an error
test_that("createRankMatrixBarPlot for mlr benchmark", {
  p = createRankMatrixBarPlot(mlr.benchmark.example, "measure.ber.test.mean")
  expect_true(is.list(p))
})

# Check if creating a Rank Matrix Bar Plot throws an error
test_that("createRankMatrixBarPlot for microbenchmark", {
  p = createRankMatrixBarPlot(microbenchmark.example)
  expect_true(is.list(p))
})
