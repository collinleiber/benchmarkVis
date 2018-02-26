context("CSV Handler")

# Check if exporting and importing a csv file works properly
test_that("CSV Export/Import Test", {
  csvExport(mlr.benchmark.example, "test.csv")
  dt = csvImport("test.csv")
  file.remove("test.csv")
  # Workarount to avoid difference between named empty list and list()
  dt2 = cbind(mlr.benchmark.example)
  dt2$algorithm.parameter = sapply(mlr.benchmark.example$algorithm.parameter, function(x) {
    # Check if list is empty
    if (!length(x)) {
      return(list())
    }
    return(x)
  })
  expect_equal(dt, dt2)
})
