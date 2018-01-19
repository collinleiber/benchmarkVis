context("CSV Handler")

#  ===================== Basic Setup =====================
csvExport(mlr.benchmark.example, "test.csv")
df = csvImport("test.csv")
file.remove("test.csv")
#  =======================================================

# Check if exporting and importing a csv file works properly
test_that("CSV Export/Import Test", {
  # Workarount to avoid difference between named empty list and list()
  df2 = cbind(mlr.benchmark.example)
  df2$algorithm.parameter = sapply(mlr.benchmark.example$algorithm.parameter, function(x) {
    # Chheck if list is empty
    if (!length(x)) {
      return(list())
    }
    return(x)
  })
  # Identical?
  expect_equal(df, df2, tolerance = 0.000000001)
})

# Check dataframe structure
test_that("CSV Import Structure", {
  # Check structure
  expect_true(checkStructure(df))
})
