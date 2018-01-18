context("Replication Line Chart")

# Check if creating a replication line chart throws an error
test_that("createReplicationLineChart Test mlr benchmark", {
  p = createReplicationLinePlot(mlr.benchmark.example, "replication.ber")
  expect_true(is.list(p))
})

# Check if creating a replication line chart throws an error
test_that("createReplicationLineChart Test microbenchmark", {
  p = createReplicationLinePlot(microbenchmark.example, "replication.values")
  expect_true(is.list(p))
})
