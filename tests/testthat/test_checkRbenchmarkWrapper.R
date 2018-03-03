context("Rbenchmark Wrapper")

# Check if wrapped rbenchmak object equals rbenchmark.example
test_that("RbenchmarkWrapper Test", {
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
  dt = useRbenchmarkWrapper(benchmark)
  # Check if columns are in data table
  expect_true("measure.elapsed" %in% colnames(dt) &&
      is.numeric(dt$measure.elapsed))
  expect_true("measure.relative" %in% colnames(dt) &&
      is.numeric(dt$measure.relative))
  expect_true("measure.user.self" %in% colnames(dt) &&
      is.numeric(dt$measure.user.self))
  expect_true("measure.sys.self" %in% colnames(dt) &&
      is.numeric(dt$measure.sys.self))
  expect_true("measure.user.child" %in% colnames(dt) &&
      is.numeric(dt$measure.user.child))
  expect_true("measure.sys.child" %in% colnames(dt) &&
      is.numeric(dt$measure.sys.child))
  # Remove checked columns (can not compare execution time)
  dt = subset(dt, select = -c(measure.elapsed, measure.relative, measure.user.self, measure.sys.self, measure.user.child, measure.sys.child))
  dt2 = subset(rbenchmark.example,
    select = -c(measure.elapsed, measure.relative, measure.user.self, measure.sys.self, measure.user.child, measure.sys.child))
  # Identical?
  expect_identical(dt, dt2)
})
