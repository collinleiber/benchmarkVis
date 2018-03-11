context("Dot Plot")

# Check if creating a Dot plot throws an error
test_that("createScatterPlot for mlr benchmark", {
  p = createScatterPlot(mlr.benchmark.example, "measure.mmce.test.mean", "measure.ber.test.mean")
  expect_true(is.list(p))
})

# Check if creating a Dot Plot throws an error
test_that("createScatterPlot for microbenchmark", {
  p = createScatterPlot(microbenchmark.example, "measure.mean", "measure.median")
  expect_true(is.list(p))
})
