context("Iteration Plots")

# Check if creating a iteration parameter plot throws an error
test_that("createIterationParameterPlot Test", {
  p = createIterationParameterPlot(mlr.tuning.example, "measure.acc.test.mean", "C")
  expect_true(is.list(p))
})

# Check if creating a iteration dual parameter plot throws an error
test_that("createIterationDualParameterPlot Test", {
  p = createIterationDualParameterPlot(mlr.tuning.example, "measure.acc.test.mean", "C", "sigma")
  expect_true(is.list(p))
})

# Check if creating a iteration plot throws an error
test_that("createIterationPlot Test", {
  p = createIterationPlot(mlr.tuning.example, "measure.acc.test.mean", "mean", TRUE, "classif.ksvm", "C")
  expect_true(is.list(p))
})
