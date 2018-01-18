context("Microbenchmark Wrapper")

# Check if wrapped microbenchmark object equals microbenchmark.example
test_that("MicrobenchmarkWrapper Test", {
  set.seed(2017)
  # Create microbenchmark
  x = runif(100)
  benchmark = microbenchmark(
    shell_sort = sort(x, method = "shell"),
    quick_sort = sort(x, method = "quick"),
    radix_sort = sort(x, method = "radix"),
    unit = "eps",
    times = 50,
    control = list(order = "inorder", warmupt = 5)
  )
  df = useMicrobenchmarkWrapper(benchmark)
  # Check if columns are in dataframe
  expect_true("min" %in% colnames(df) && is.numeric(df$min))
  expect_true("lq" %in% colnames(df) && is.numeric(df$lq))
  expect_true("mean" %in% colnames(df) && is.numeric(df$mean))
  expect_true("median" %in% colnames(df) && is.numeric(df$median))
  expect_true("uq" %in% colnames(df) && is.numeric(df$uq))
  expect_true("max" %in% colnames(df) && is.numeric(df$max))
  expect_true("replication.values" %in% colnames(df) && is.vector(df$replication.values))
  # Remove checked columns (can not compare execution time)
  df = subset(df, select = -c(min, lq, mean, median, uq, max, replication.values))
  df2 = subset(microbenchmark.example,
    select = -c(min, lq, mean, median, uq, max, replication.values))
  # Identical?
  expect_identical(df, df2)
})

# Check dataframe structure
test_that("MicrobenchmarkWrapper Structure", {
  set.seed(2017)
  # Create microbenchmark
  x = runif(100)
  benchmark = microbenchmark(
    shell_sort = sort(x, method = "shell"),
    quick_sort = sort(x, method = "quick"),
    radix_sort = sort(x, method = "radix"),
    unit = "eps",
    times = 50,
    control = list(order = "inorder", warmupt = 5)
  )
  df = useMicrobenchmarkWrapper(benchmark)
  # Check Structure
  expect_true(checkStructure(df))
})
