context("Parallel Coordinates Plot")

# Check if creating a Parallel Coordinates plot throws an error
test_that("createParallelCoordinatesPlot for mlr benchmark", {
  p = createParallelCoordinatesPlot(mlr.benchmark.example)
  expect_true(is.list(p))
})

# Check if creating a Rank Plot throws an error
test_that("createParallelCoordinatesPlot for microbenchmark", {
  p = createParallelCoordinatesPlot(microbenchmark.example)
  expect_true(is.list(p))
})
