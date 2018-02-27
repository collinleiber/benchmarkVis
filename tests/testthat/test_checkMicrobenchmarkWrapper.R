context("Microbenchmark Wrapper")

# Check if wrapped microbenchmark object equals microbenchmark.example
test_that("MicrobenchmarkWrapper Test", {
  set.seed(2017)
  x = runif(100)
  # Create microbenchmark
  benchmark = microbenchmark::microbenchmark(
    shell_sort = sort(x, method = "shell"),
    quick_sort = sort(x, method = "quick"),
    radix_sort = sort(x, method = "radix"),
    unit = "eps",
    times = 50,
    control = list(order = "inorder", warmupt = 5)
  )
  dt = useMicrobenchmarkWrapper(benchmark)
  # Check if columns are in data table
  expect_true("measure.min" %in% colnames(dt) && is.numeric(dt$measure.min))
  expect_true("measure.lq" %in% colnames(dt) && is.numeric(dt$measure.lq))
  expect_true("measure.mean" %in% colnames(dt) && is.numeric(dt$measure.mean))
  expect_true("measure.median" %in% colnames(dt) && is.numeric(dt$measure.median))
  expect_true("measure.uq" %in% colnames(dt) && is.numeric(dt$measure.uq))
  expect_true("measure.max" %in% colnames(dt) && is.numeric(dt$measure.max))
  expect_true("list.values" %in% colnames(dt) && is.vector(dt$list.values))
  # Remove checked columns (can not compare execution time)
  dt = subset(dt, select = -c(measure.min, measure.lq, measure.mean, measure.median, measure.uq, measure.max, list.values))
  dt2 = subset(microbenchmark.example,
    select = -c(measure.min, measure.lq, measure.mean, measure.median, measure.uq, measure.max, list.values))
  # Identical?
  expect_identical(dt, dt2)
})
