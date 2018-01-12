# Check if wrapped benchmak object equals example.rds object
test_that("Wrapper of mlr benchmark objects produces correct output", {
  df1 = makeMlrBenchmarkWrapper("../../data/mlrBenchmark.rds")
  df2 = readRDS("../../data/Example.rds")
  identical(df1, df2)
})
