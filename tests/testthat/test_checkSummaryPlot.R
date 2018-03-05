context("Summary Plot")

# Check if creating a Summary plot throws an error
test_that("createSummaryPlot for mlr benchmark", {
  p = createSummaryPlot(mlr.benchmark.example, "measure.mmce.test.mean")
  expect_true(is.list(p))
})

# Check if creating a Summary Plot throws an error
test_that("createSummaryPlot for microbenchmark", {
  p = createSummaryPlot(microbenchmark.example, "measure.mean")
  expect_true(is.list(p))
})
