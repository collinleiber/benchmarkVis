context("Replication plots")

# Check if creating a replication line plot throws an error
test_that("createReplicationLinePlot Test mlr benchmark", {
  p = createReplicationLinePlot(mlr.benchmark.example, "list.ber")
  expect_true(is.list(p))
})

# Check if creating a replication line plot throws an error
test_that("createReplicationLinePlot Test microbenchmark", {
  p = createReplicationLinePlot(microbenchmark.example, "list.values")
  expect_true(is.list(p))
})
