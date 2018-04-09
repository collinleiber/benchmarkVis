context("Measure Matrix Plot")

# Check if creating a Measure Matrix plot throws an error
test_that("createMeasureMatrixPlot for mlr benchmark", {
  p = createMeasureMatrixPlot(mlr.benchmark.example)
  expect_true(is.list(p))
})

# Check if creating a Measure Matrix plot throws an error
test_that("createMeasureMatrixPlot for microbenchmark", {
  p = createMeasureMatrixPlot(microbenchmark.example, "problem")
  expect_true(is.list(p))
})
