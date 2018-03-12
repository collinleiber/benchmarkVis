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
