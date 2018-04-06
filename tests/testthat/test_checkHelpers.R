context("Helpers")

# Check if getMeasures() for mlr benchmark is working correctly
test_that("getMeasures for mlr benchmark", {
  measures = getMeasures(mlr.benchmark.example)
  expect_identical(measures, c("measure.mmce.test.mean", "measure.ber.test.mean", "measure.timetrain.test.mean"))
})

# Check if getMeasuresCount() for microbenchmark is working correctly
test_that("getMeasuresCount for microbenchmark", {
  count = getMeasuresCount(microbenchmark.example)
  expect_equal(count, 6)
})

# Check if getLists() for mlr benchmark is working correctly
test_that("getLists for mlr benchmark", {
  measures = getLists(mlr.benchmark.example)
  expect_identical(measures, c("list.mmce", "list.ber", "list.timetrain"))
})

# Check if getListsCount() for microbenchmark is working correctly
test_that("getListsCount for microbenchmark", {
  count = getListsCount(microbenchmark.example)
  expect_equal(count, 1)
})

# Check if getMainColumns() for mlr benchmark is working correctly
test_that("getMainColumns for mlr benchmark", {
  main.columns = getMainColumns(mlr.benchmark.example)
  expect_identical(main.columns, c("problem", "algorithm", "replication"))
})

# Check if getMainColumnsCount() for microbenchmark is working correctly
test_that("getMainColumnsCount for microbenchmark", {
  count = getMainColumnsCount(microbenchmark.example)
  expect_equal(count, 3)
})

# Check if getParameterColumns() for mlr benchmark is working correctly
test_that("getParameterColumns for mlr benchmark", {
  parameter.columns = getParameterColumns(mlr.benchmark.example)
  expect_identical(parameter.columns, c("problem.parameter", "algorithm.parameter", "replication.parameter"))
})

# Check if getParameterColumnsCount() for microbenchmark is working correctly
test_that("getParameterColumnsCount for microbenchmark", {
  count = getParameterColumnsCount(microbenchmark.example)
  expect_equal(count, 2)
})

# Check if getIterationAlgorithms() for mlr tuning is working correctly
test_that("getIterationAlgorithms for mlr tuning", {
  measures = getIterationAlgorithms(mlr.tuning.example)
  expect_identical(measures, "classif.ksvm")
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
