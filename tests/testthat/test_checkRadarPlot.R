context("Radar Plot")

# Check if creating a Radar plot throws an error
test_that("createRadarPlot for mlr benchmark", {
  p = createRadarPlot(mlr.benchmark.example)
  expect_true(is.list(p))
})

# Check if creating a Radar throws an error
test_that("createRadarPlot for microbenchmark", {
  tmp = microbenchmark.example
  tmp$replication = NULL
  p = createRadarPlot(tmp)
  expect_true(is.list(p))
})
