context("Scatter Plot")

# Check if creating a Scatter plot throws an error
test_that("createScatterPlot for mlr benchmark", {
  p = createScatterPlot(mlr.benchmark.example, "measure.mmce.test.mean", "measure.ber.test.mean")
  expect_true(is.list(p))
})

# Check if creating a Scatter Plot throws an error
test_that("createScatterPlot for microbenchmark", {
  p = createScatterPlot(microbenchmark.example, "measure.mean", "measure.median", FALSE)
  expect_true(is.list(p))
})
