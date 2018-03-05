context("Rank Plot")

# Check if creating a Rank plot throws an error
test_that("createRankPlot for mlr benchmark", {
  p = createRankPlot(mlr.benchmark.example, "measure.mmce.test.mean")
  expect_true(is.list(p))
})

# Check if creating a Rank Plot throws an error
test_that("createRankPlot for microbenchmark", {
  p = createRankPlot(microbenchmark.example, "measure.mean")
  expect_true(is.list(p))
})
