context("Do Aggregation")

# Check if Do aggregation  for mlr benchmark throws an error
test_that("Do aggregation for mlr benchmark", {
  agd = get.result(groupby= c("problem", "algorithm"), aggfun= c("mean"), aggcol= c("mmce.test.mean", "ber.test.mean"), df= mlr.benchmark.example)
  expect_true(is.data.frame(agd) && ncol(agd) == 4L)
  expect_true(is.numeric(agd[, 3L]) && is.numeric(agd[, 4L]))
  expect_equal(colnames(agd), c("problem", "algorithm", "mean_mmce.test.mean", "mean_ber.test.mean"))
})

# Check if Do aggregation for microbenchmark throws an error
test_that("Do aggregation for microbenchmark", {
  ag.df = get.result(groupby= c("problem", "algorithm"), aggfun= c("mean"), aggcol= c("mmce.test.mean", "ber.test.mean"), df= microbenchmark.example)
  expect_true(is.data.frame(agd) && ncol(agd) == 4L)
  expect_true(is.numeric(agd[, 3L]) && is.numeric(agd[, 4L]))
  expect_equal(colnames(agd), c("problem", "algorithm", "mean_mmce.test.mean", "mean_ber.test.mean"))
})
