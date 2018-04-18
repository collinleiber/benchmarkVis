context("Strucure Check")

# Check if checkStructure() for datasets is working correctly
test_that("checkStructure for datasets", {
  expect_true(checkStructure(mlr.benchmark.example))
  expect_true(checkStructure(microbenchmark.example))
  expect_true(checkStructure(rbenchmark.example))
  expect_true(checkStructure(large.benchmark))
  expect_true(checkStructure(mlr.tuning.example))
  tmp = mlr.tuning.example
  tmp$replication = rep("none", nrow(tmp))
  expect_true(checkStructure(tmp))
})

# Check if checkStructure() without problem or algorithm throws error
test_that("checkStructure without problem/algorithm", {
  tmp = mlr.benchmark.example
  tmp$algorithm = NULL
  expect_error(checkStructure(tmp))
  tmp = mlr.benchmark.example
  tmp$problem = NULL
  expect_error(checkStructure(tmp))
})

# Check if checkStructure() with double orrurance of iteration throws error
test_that("checkStructure double iteration", {
  tmp = mlr.tuning.example
  tmp$algrithm.parameter[[1]]$iteration = 2
  expect_error(checkStructure(tmp))
})

# Check if checkStructure() with unnown column throws error
test_that("checkStructure unknown column", {
  tmp = microbenchmark.example
  tmp$unknown = rep(2, nrow(tmp))
  expect_error(checkStructure(tmp))
})

# Check if checkStructure() without measure column throws error
test_that("checkStructure without measure column", {
  tmp = mlr.tuning.example
  tmp$measure.acc.test.mean = NULL
  tmp$measure.acc.test.sd = NULL
  expect_error(checkStructure(tmp))
})
