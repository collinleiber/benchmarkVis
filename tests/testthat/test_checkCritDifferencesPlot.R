context("CritDifferences Plot")

# Check if creating a CritDifferences plot throws an error
test_that("createCritDifferences for mlr benchmark", {
  p = createCritDifferencesPlot(mlr.benchmark.example, "measure.mmce.test.mean")
  expect_true(is.list(p))
})
