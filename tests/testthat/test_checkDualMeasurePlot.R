context("Dual Measure Plot")

# Check if creating a Dual Measure plot throws an error
test_that("createDualMeasurePlot for mlr benchmark", {
  p = createDualMeasurePlot(mlr.benchmark.example, "measure.mmce.test.mean", "measure.ber.test.mean")
  expect_true(is.list(p))
})

# Check if creating a Dual Measure Plot throws an error
test_that("createDualMeasurePlot for microbenchmark", {
  p = createDualMeasurePlot(microbenchmark.example, "measure.mean", "measure.median", FALSE)
  expect_true(is.list(p))
})
