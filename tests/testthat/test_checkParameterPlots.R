context("Parameter Plots")

# Check if creating a parameter scatter plot throws an error
test_that("createParameterScatterPlot Test", {
  p = createParameterScatterPlot(mlr.tuning.example, "measure.acc.test.mean", "algorithm.parameter", "C")
  expect_true(is.list(p))
})

# Check if creating a parameter scatter plot with histogram throws an error
test_that("createParameterScatterPlot with histogram Test", {
  p = createParameterScatterPlot(mlr.tuning.example, "measure.acc.test.mean", "algorithm.parameter", "C", TRUE, TRUE)
  expect_true(is.list(p))
})

# Check if creating a parameter dual plot throws an error
test_that("createParameterDualPlot Test", {
  p = createParameterDualPlot(mlr.tuning.example, "measure.acc.test.mean", "algorithm.parameter", "C", "algorithm.parameter", "sigma")
  expect_true(is.list(p))
})

# Check if creating a parameter dual with area plot throws an error
test_that("createParameterDualPlot with area Test", {
  p = createParameterDualPlot(mlr.tuning.example, "measure.acc.test.mean", "algorithm.parameter", "C", "algorithm.parameter", "sigma", TRUE)
  expect_true(is.list(p))
})

# Check if creating a parameter box plot throws an error
test_that("createParameterBoxPlot Test mlr tuning", {
  p = createParameterBoxPlot(mlr.tuning.example, "measure.acc.test.mean", "algorithm.parameter", "C")
  expect_true(is.list(p))
})

# Check if creating a parameter box plot with violine throws an error
test_that("createParameterBoxPlot Test with lines mlr tuning", {
  p = createParameterBoxPlot(mlr.tuning.example, "measure.acc.test.mean", "algorithm.parameter", "C", TRUE)
  expect_true(is.list(p))
})

# Check if creating a parameter density plot throws an error
test_that("createParameterDensityPlot Test mlr tuning", {
  p = createParameterDensityPlot(mlr.tuning.example, "measure.acc.test.mean", "algorithm.parameter", "C")
  expect_true(is.list(p))
})

# Check if creating a parameter density plot stacked throws an error
test_that("createParameterDensityPlot stacked Test mlr tuning", {
  p = createParameterDensityPlot(mlr.tuning.example, "measure.acc.test.mean", "algorithm.parameter", "C", TRUE)
  expect_true(is.list(p))
})

# Check if creating a parameter measure matrix plot throws an error
test_that("createParameterMeasureMatrixPlot Test mlr tuning", {
  p = createParameterMeasureMatrixPlot(mlr.tuning.example, "algorithm.parameter", "C")
  expect_true(is.list(p))
})
