context("Helpers")

# Check if exporting and importing a csv file works properly
test_that("Helper functions work well", {
  dt = mlr.benchmark.example
  expect_equal(getMeasuresCount(dt), 3)

  dt = microbenchmark.example
  expect_equal(getListsCount(dt), 1)

  dt = mlr.tuning.example
  expect_equal(getIterationAlgorithmsCount(dt), 1)
})
