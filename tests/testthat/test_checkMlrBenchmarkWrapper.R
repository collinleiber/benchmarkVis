# Check if wrapped benchmak object equals example.rds object
test_that("Wrapper of mlr benchmark objects produces correct output", {
  # Load mlr benchmark result object and convert it
  data("mlr.benchmark")
  df = useMlrBenchmarkWrapper(mlr.benchmark)
  # Load benchmarkVis specific df
  data("ml.example")
  identical(df, ml.example)
})
