context("Dot Plot")

# Check if creating a Dot plot throws an error
test_that("createDotPlot for mlr benchmark", {
  p = createDotPlot(mlr.benchmark.example, "measure.mmce.test.mean", "measure.ber.test.mean")
  expect_true(is.list(p))
})

# Check if creating a Dot Plot throws an error
test_that("createDotPlot for microbenchmark", {
  p = createDotPlot(microbenchmark.example, "measure.mean", "measure.median")
  expect_true(is.list(p))
})
