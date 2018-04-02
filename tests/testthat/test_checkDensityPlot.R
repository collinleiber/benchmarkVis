context("Density Plot")

# Check if creating a stacked Density Plot throws an error
test_that("createDensityPlot for mlr benchmark stacked", {
  p = createDensityPlot(mlr.benchmark.example, "measure.mmce.test.mean", TRUE)
  expect_true(is.list(p))
})

# Check if creating a Density Plot throws an error
test_that("createDensityPlot for mlr benchmark", {
  p = createDensityPlot(mlr.benchmark.example, "measure.mmce.test.mean", FALSE)
  expect_true(is.list(p))
})
