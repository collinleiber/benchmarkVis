context("Box Plot")

# Check if creating a Box Plot throws an error
test_that("createBoxPlot for microbenchmark", {
  p = createBoxPlot(microbenchmark.example, "measure.mean")
  expect_true(is.list(p))
})

# Check if creating a Box Plot with violin throws an error
test_that("createBoxPlot for mlr benchmark with violin", {
  p = createBoxPlot(mlr.benchmark.example, "measure.mmce.test.mean", violin = TRUE)
  expect_true(is.list(p))
})

# Check if creating a Box Plot with facetting.problem throws an error
# test_that("createBoxPlot for mlr benchmark with facetting.problem", {
#   p = createBoxPlot(mlr.benchmark.example, "measure.mmce.test.mean", facetting.problem = TRUE)
#   expect_true(is.list(p))
# })
