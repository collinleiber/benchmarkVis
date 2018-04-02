context("Density Rank Plot")

# Check if creating a stacked Density Rank Plot throws an error
test_that("createDensityRankPlot for mlr benchmark stacked", {
  p = createDensityRankPlot(mlr.benchmark.example, "measure.mmce.test.mean", TRUE)
  expect_true(is.list(p))
})

# Check if creating a Density Rank Plot throws an error
test_that("createDensityRankPlot for mlr benchmark", {
  p = createDensityRankPlot(mlr.benchmark.example, "measure.mmce.test.mean", FALSE)
  expect_true(is.list(p))
})
