context("Rbenchmark Wrapper")

#  ===================== Basic Setup =====================
set.seed(2017)
# Create rbenchmark
benchmark = rbenchmark::benchmark(
  shell_sort = sort(runif(1000), method = "shell"),
  quick_sort = sort(runif(1000), method = "quick"),
  radix_sort = sort(runif(1000), method = "radix"),
  columns = c(
    "test", "replications", "elapsed", "relative", "user.self", "sys.self",
    "user.child", "sys.child"),
  order = "test",
  replications = c(100, 20),
  environment = parent.frame(),
  relative = "elapsed"
)
df = useRbenchmarkWrapper(benchmark)
#  =======================================================

# Check if wrapped rbenchmak object equals rbenchmark.example
test_that("RbenchmarkWrapper Test", {
  # Check if columns are in dataframe
  expect_true("elapsed" %in% colnames(df) &&
      is.numeric(df$elapsed))
  expect_true("user.self" %in% colnames(df) &&
      is.numeric(df$user.self))
  expect_true("sys.self" %in% colnames(df) &&
      is.numeric(df$sys.self))
  # Remove checked columns (can not compare execution time)
  df = subset(df, select = -c(elapsed, user.self, sys.self))
  df2 = subset(rbenchmark.example, select = -c(elapsed, user.self, sys.self))
  # Identical?
  expect_identical(df, df2)
})

# Check dataframe structure
test_that("RbenchmarkWrapper Structure", {
  expect_true(checkStructure(df))
})