context("Tuning Plots")

# Check if creating a tuning parameter plot throws an error
test_that("createTuningParameterPlot Test", {
  p = createTuningParameterPlot(mlr.tuning.example, "C", "acc.test.mean")
  expect_true(is.list(p))
})

# Check if creating a tuning iteration plot throws an error
test_that("createTuningIterationPlot Test", {
  p = createTuningIterationPlot(mlr.tuning.example, "acc.test.mean")
  expect_true(is.list(p))
})
