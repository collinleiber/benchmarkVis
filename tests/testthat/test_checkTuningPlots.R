context("Tuning Plots")

# Check if creating a tuning line plot throws an error
test_that("createTuningLinePlot Test", {
  p = createTuningLinePlot(mlr.tuning.example, "measure.acc.test.mean", "mean")
  expect_true(is.list(p))
})

# Check if creating a tuning line plot with parameter throws an error
test_that("createTuningLinePlot with parameter Test", {
  tmp = mlr.tuning.example
  tmp$replication = rep("none", nrow(tmp))
  p = createTuningLinePlot(tmp, "measure.acc.test.mean", "mean", TRUE, "C")
  expect_true(is.list(p))
})
