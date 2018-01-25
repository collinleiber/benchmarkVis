context("Replication plots")

# Check if creating a replication line plot throws an error
test_that("createReplicationLinePlot Test mlr benchmark", {
  p = createReplicationLinePlot(mlr.benchmark.example, "replication.ber")
  expect_true(is.list(p))
})

# Check if creating a replication line plot throws an error
test_that("createReplicationLinePlot Test microbenchmark", {
  p = createReplicationLinePlot(microbenchmark.example, "replication.values")
  expect_true(is.list(p))
})
