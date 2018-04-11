context("Iteration Plots")

# Check if creating a iteration parameter plot throws an error
test_that("createIterationParameterPlot Test", {
  p = createIterationParameterPlot(mlr.tuning.example, "measure.acc.test.mean", "C")
  expect_true(is.list(p))
})

# Check if creating a iteration parameter plot with histogram throws an error
test_that("createIterationParameterPlot with histogram Test", {
  p = createIterationParameterPlot(mlr.tuning.example, "measure.acc.test.mean", "C", TRUE, TRUE)
  expect_true(is.list(p))
})

# Check if creating a iteration dual parameter plot throws an error
test_that("createIterationDualParameterPlot Test", {
  p = createIterationDualParameterPlot(mlr.tuning.example, "measure.acc.test.mean", "C", "sigma")
  expect_true(is.list(p))
})

# Check if creating a iteration dual parameter with area plot throws an error
test_that("createIterationDualParameterPlot with area Test", {
  p = createIterationDualParameterPlot(mlr.tuning.example, "measure.acc.test.mean", "C", "sigma", TRUE)
  expect_true(is.list(p))
})

# Check if creating a iteration line plot throws an error
test_that("createIterationLinePlot Test", {
  p = createIterationLinePlot(mlr.tuning.example, "measure.acc.test.mean", "mean", TRUE)
  expect_true(is.list(p))
})

# Check if creating a iteration line plot with parameter throws an error
test_that("createIterationLinePlot with parameter Test", {
  p = createIterationLinePlot(mlr.tuning.example, "measure.acc.test.mean", "mean", TRUE, "classif.ksvm", "C")
  expect_true(is.list(p))
})

# Check if creating a iteration dual measure plot throws an error
test_that("createIterationDualMeasurePlot Test mlr tuning", {
  p = createIterationDualMeasurePlot(mlr.tuning.example, "measure.acc.test.mean", "measure.acc.test.sd")
  expect_true(is.list(p))
})

# Check if creating a iteration dual measure plot with lines throws an error
test_that("createIterationDualMeasurePlot Test with lines mlr tuning", {
  p = createIterationDualMeasurePlot(mlr.tuning.example, "measure.acc.test.mean", "measure.acc.test.sd", TRUE, TRUE)
  expect_true(is.list(p))
})

# Check if creating a iteration scatter plot throws an error
test_that("createIterationScatterPlot Test mlr tuning", {
  p = createIterationScatterPlot(mlr.tuning.example, "measure.acc.test.mean")
  expect_true(is.list(p))
})

# Check if creating a iteration box plot throws an error
test_that("createIterationBoxPlot Test mlr tuning", {
  p = createIterationBoxPlot(mlr.tuning.example, "measure.acc.test.mean")
  expect_true(is.list(p))
})

# Check if creating a iteration box plot with violine throws an error
test_that("createIterationBoxPlot Test with lines mlr tuning", {
  p = createIterationBoxPlot(mlr.tuning.example, "measure.acc.test.mean", TRUE)
  expect_true(is.list(p))
})

# Check if creating a iteration density plot throws an error
test_that("createIterationDensityPlot Test mlr tuning", {
  p = createIterationDensityPlot(mlr.tuning.example, "measure.acc.test.mean")
  expect_true(is.list(p))
})

# Check if creating a iteration measure matrix plot throws an error
test_that("createIterationMeasureMatrixPlot Test mlr tuning", {
  p = createIterationMeasureMatrixPlot(mlr.tuning.example)
  expect_true(is.list(p))
})
