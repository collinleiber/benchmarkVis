context("Radar Plot")

# Check if creating a Radar plot throws an error
test_that("createRadarPlot for mlr benchmark", {
  p = createRadarPlot(mlr.benchmark.example)
  expect_true(is.list(p))
})

# Check if creating a Radar throws an error
test_that("createRadarPlot for microbenchmark", {
  p = createRadarPlot(microbenchmark.example)
  expect_true(is.list(p))
})
