context("Information")

# Check if listPlots() is working correctly
test_that("listPlots test", {
  plots = listPlots()
  expect_true(is.vector(plots))
  expect_true(length(plots) > 0)
  expect_true(all(sapply(plots, function(x) {
    startsWith(x, "create") && endsWith(x, "Plot")
  })))
})

# Check if listWrappers() is working correctly
test_that("listWrappers test", {
  wrappers = listWrappers()
  expect_true(is.vector(wrappers))
  expect_true(length(wrappers) > 0)
  expect_true(all(sapply(wrappers, function(x) {
    startsWith(x, "use") && endsWith(x, "Wrapper")
  })))
})

# Check if getValidPlots() for mlr benchmark and microbenchmark is working correctly
test_that("getValidPlots for mlr benchmark compared to microbenchmark", {
  plots = getValidPlots(mlr.benchmark.example)
  expect_true(length(plots) > 1)
  expect_true(length(plots) == length(getValidPlots(microbenchmark.example)))
})

# Check if getValidPlots() for mlr benchmark and rbenchmark is working correctly
test_that("getValidPlots for mlr benchmark compared to rbenchmark", {
  plots = getValidPlots(mlr.benchmark.example)
  expect_true(length(plots) > 1)
  expect_true(length(plots) > length(getValidPlots(rbenchmark.example)))
})

# Check if getValidPlots() for mlr tuning and rbenchmark is working correctly
test_that("getValidPlots for mlr tuning compared to rbenchmark", {
  plots = getValidPlots(mlr.tuning.example)
  expect_true(length(plots) > 1)
  expect_true(length(plots) > length(getValidPlots(rbenchmark.example)))
})

# Check if getMeasures() for mlr benchmark is working correctly
test_that("getMeasures for mlr benchmark", {
  measures = getMeasures(mlr.benchmark.example)
  expect_identical(measures, c("measure.mmce.test.mean", "measure.ber.test.mean", "measure.timetrain.test.mean"))
})

# Check if getLists() for mlr benchmark is working correctly
test_that("getLists for mlr benchmark", {
  lists = getLists(mlr.benchmark.example)
  expect_identical(lists, c("list.mmce", "list.ber", "list.timetrain"))
})

# Check if getMainColumns() for mlr benchmark is working correctly
test_that("getMainColumns for mlr benchmark", {
  main.columns = getMainColumns(mlr.benchmark.example)
  expect_identical(main.columns, c("problem", "algorithm", "replication"))
})

# Check if getParameterColumns() for mlr benchmark is working correctly
test_that("getParameterColumns for mlr benchmark", {
  parameter.columns = getParameterColumns(mlr.benchmark.example)
  expect_identical(parameter.columns, c("problem.parameter", "algorithm.parameter", "replication.parameter"))
})

# Check if getTunings() for mlr tuning is working correctly
test_that("getTunings for mlr tuning", {
  tuning = getTunings(mlr.tuning.example)
  expect_identical(tuning, list(c("TuneControlRandom", "classif.ksvm"), c("TuneControlGrid", "classif.ksvm")))
})

# Check if getTunings() for mlr tuning is working correctly
test_that("getTunings for mlr tuning with replication", {
  tmp = mlr.tuning.example
  tmp$replication = rep("none", nrow(tmp))
  tuning = getTunings(tmp)
  expect_identical(tuning, list(c("TuneControlRandom", "classif.ksvm", "none"), c("TuneControlGrid", "classif.ksvm", "none")))
})
