context("List plots")

# Check if creating a list line plot with histogram throws an error
test_that("createListLinePlot Test mlr benchmark with histogram", {
  p = createListLinePlot(mlr.benchmark.example, "list.ber", "mean", TRUE)
  expect_true(is.list(p))
})

# Check if creating a list line plot throws an error
test_that("createListLinePlot Test microbenchmark", {
  tmp = microbenchmark.example
  tmp$replication = NULL
  p = createListLinePlot(tmp, "list.values")
  expect_true(is.list(p))
})

# Check if creating a list density plot throws an error
test_that("createListDensityPlot Test mlr benchmark", {
  p = createListDensityPlot(mlr.benchmark.example, "list.ber", TRUE)
  expect_true(is.list(p))
})

# Check if creating a list density plot throws an error
test_that("createListDensityPlot Test microbenchmark", {
  tmp = microbenchmark.example
  tmp$replication = NULL
  p = createListDensityPlot(tmp, "list.values")
  expect_true(is.list(p))
})

# Check if creating a list density rank plot throws an error
test_that("createListDensityRankPlot Test mlr benchmark", {
  p = createListDensityRankPlot(mlr.benchmark.example, "list.ber", TRUE)
  expect_true(is.list(p))
})

# Check if creating a list density rank plot throws an error
test_that("createListDensityRankPlot Test microbenchmark", {
  tmp = microbenchmark.example
  tmp$replication = NULL
  p = createListDensityRankPlot(tmp, "list.values")
  expect_true(is.list(p))
})

# Check if creating a list rank matrix bar plot throws an error
test_that("createListRankMatrixBarPlot Test mlr benchmark", {
  p = createListRankMatrixBarPlot(mlr.benchmark.example, "list.ber")
  expect_true(is.list(p))
})

# Check if creating a list rank matrix bar plot throws an error
test_that("createListRankMatrixBarPlot Test microbenchmark", {
  tmp = microbenchmark.example
  tmp$replication = NULL
  p = createListRankMatrixBarPlot(tmp, "list.values")
  expect_true(is.list(p))
})

# Check if creating a list dual measure plot throws an error
test_that("createListDualMeasurePlot Test mlr benchmark", {
  p = createListDualMeasurePlot(mlr.benchmark.example, "list.mmce", "list.timetrain")
  expect_true(is.list(p))
})

# Check if creating a list dual measure plot with lines throws an error
test_that("createListDualMeasurePlot Test with lines mlr benchmark", {
  tmp = mlr.benchmark.example
  tmp$replication = NULL
  p = createListDualMeasurePlot(tmp, "list.mmce", "list.ber", TRUE)
  expect_true(is.list(p))
})
