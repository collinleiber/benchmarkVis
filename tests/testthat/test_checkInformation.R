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
