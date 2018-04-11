context("Helpers")

# Check if getMeasuresCount() for microbenchmark is working correctly
test_that("getMeasuresCount for microbenchmark", {
  count = getMeasuresCount(microbenchmark.example)
  expect_equal(count, 6)
})

# Check if getListsCount() for microbenchmark is working correctly
test_that("getListsCount for microbenchmark", {
  count = getListsCount(microbenchmark.example)
  expect_equal(count, 1)
})

# Check if getMainColumnsCount() for microbenchmark is working correctly
test_that("getMainColumnsCount for microbenchmark", {
  count = getMainColumnsCount(microbenchmark.example)
  expect_equal(count, 3)
})

# Check if getParameterColumnsCount() for microbenchmark is working correctly
test_that("getParameterColumnsCount for microbenchmark", {
  count = getParameterColumnsCount(microbenchmark.example)
  expect_equal(count, 2)
})

# Check if getIterationAlgorithmsCount() for mlr tuning is working correctly
test_that("getIterationAlgorithmsCount  mlr tuning", {
  count = getIterationAlgorithmsCount(mlr.tuning.example)
  expect_equal(count, 1)
})

# Check if getCumulativeValues() min is working correctly
test_that("getCumulativeValues  min", {
  values = getCumulativeValues(c(2, 5, 3, 7, 1, 4), "min")
  expect_equal(values, c(2, 2, 2, 2, 1, 1))
})

# Check if getCumulativeValues() id is working correctly
test_that("getCumulativeValues  id", {
  values = getCumulativeValues(c(2, 5, 3, 7, 1, 4), "id")
  expect_equal(values, c(2, 5, 3, 7, 1, 4))
})

# Check if getCumulativeValues() max is working correctly
test_that("getCumulativeValues  max", {
  values = getCumulativeValues(c(2, 5, 3, 7, 1, 4), "max")
  expect_equal(values, c(2, 5, 5, 7, 7, 7))
})

# Check if getCumulativeValues() mean is working correctly
test_that("getCumulativeValues  mean", {
  values = getCumulativeValues(c(2, 4, 3, 3, 8), "mean")
  expect_equal(values, c(2, 3, 3, 3, 4))
})

# Check if getPrettyPlotList() is working correctly
test_that("getPrettyPlotList test", {
  plots = getPrettyPlotList(listPlots())
  expect_true(is.vector(plots))
  expect_true(length(plots) > 0)
  expect_true(all(sapply(plots, function(x) {
    startsWith(x, "Measure: ") ||
      startsWith(x, "List: ") || startsWith(x, "Iteration: ")
  })))
})

# Check if getPrettyPlotName() is working correctly
test_that("getPrettyPlotName test", {
  expect_equal(getPrettyPlotName("createScatterPlot"), "Measure: Scatter Plot")
  expect_equal(getPrettyPlotName("createIterationDualParameterPlot"), "Iteration: Dual Parameter Plot")
  expect_equal(getPrettyPlotName("createListLinePlot"), "List: Line Plot")
  expect_equal(getPrettyPlotName("createListDualMeasurePlot"), "List: Dual Measure Plot")
  expect_equal(getPrettyPlotName("createIterationDualMeasurePlot"), "Iteration: Dual Measure Plot")
})

# Check if unprettifyPlotName() is working correctly
test_that("unprettifyPlotName test", {
  expect_equal(unprettifyPlotName("Measure: Scatter Plot"), "createScatterPlot")
  expect_equal(unprettifyPlotName("Iteration: Dual Parameter Plot"), "createIterationDualParameterPlot")
  expect_equal(unprettifyPlotName("List: Line Plot"), "createListLinePlot")
  expect_equal(unprettifyPlotName("List: Dual Measure Plot"), "createListDualMeasurePlot")
  expect_equal(unprettifyPlotName("Iteration: Dual Measure Plot"), "createIterationDualMeasurePlot")
})

# Check if repList is working correctly
test_that("repList  empty", {
  test.list = repList(list(), 3)
  expect_identical(list(list(), list(), list()), test.list)
})

# Check if repList is working correctly
test_that("repList  filled", {
  test.list = repList(list(a = 1, b = "x"), 3)
  expect_identical(list(list(a = 1, b = "x"), list(a = 1, b = "x"), list(a = 1, b = "x")), test.list)
})

# Check if getPrettyMeasureName is working correctly
test_that("measure names without prefix", {
  measure.name = "measure.mmce.test.mean"
  name = getPrettyMeasureName(measure.name)
  expect_equal("mmce.test.mean", name)

  measure.name = "list.mmce.test.mean"
  name = getPrettyListMeasureName(measure.name)
  expect_equal("mmce.test.mean", name)
})
