context("Box Plot")

# Check if creating a Box Plot throws an error
test_that("createBoxPlot for mlr benchmark", {
  p = createBoxPlot(mlr.benchmark.example, "mmce.test.mean")
  expect_true(is.list(p))
})

# Check if creating a Box Plot throws an error
test_that("createBoxPlot for microbenchmark", {
  p = createBoxPlot(microbenchmark.example, "mean")
  expect_true(is.list(p))
})
