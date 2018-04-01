context("Transformation")

test_that("transformation function value-to-value is valid", {
  test.fun.1 = function(x) {
    x * 2
  }
  test.fun.2 = function(x)
    1
  test.fun.3 = function(x)
    "test"
  test.fun.4 = function(x)
    c(x, x * 2)
  transform.fun.list = list(log, test.fun.1, test.fun.2)
  no.transform.fun.list = list(test.fun.3, test.fun.4)
  for (fun in transform.fun.list) {
    expect_true(check.transform.value.to.value(fun))
  }
  for (fun in no.transform.fun.list) {
    expect_false(check.transform.value.to.value(fun))
  }
})

test_that("transformation function list-to-value is valid", {
  test.fun.1 = function(x) {
    mean(x)
  }
  test.fun.2 = function(x) {
    sum(x)
  }
  test.fun.3 = function(x)
    "test"
  test.fun.4 = function(x) {
    x * 2
  }
  transform.fun.list = list(test.fun.1, test.fun.2)
  no.transform.fun.list = list(test.fun.3, test.fun.4)
  for (fun in transform.fun.list) {
    expect_true(check.transform.list.to.value(fun))
  }
  for (fun in no.transform.fun.list) {
    expect_false(check.transform.list.to.value(fun))
  }
})

test_that("transformation function list-to-list is valid", {
  test.fun.1 = function(x) {
    x * 2
  }
  test.fun.2 = function(x)
    1
  test.fun.3 = function(x)
    "test"
  test.fun.4 = function(x)
    c(x, x * 2)
  transform.fun.list = list(log, test.fun.1, test.fun.2)
  no.transform.fun.list = list(test.fun.3, test.fun.4)
  for (fun in transform.fun.list) {
    expect_true(check.transform.list.to.list(fun))
  }
  for (fun in no.transform.fun.list) {
    expect_false(check.transform.list.to.list(fun))
  }
})

test_that("Do rank and log2 transformation for mlr benchmark", {
  result = transformation.apply(
    original.data = mlr.benchmark.example,
    columns.to.transform = c("measure.mmce.test.mean", "measure.ber.test.mean"),
    transformation.functions = c("rank", "log2")
  )
  size.expected = ncol(mlr.benchmark.example) + 4L
  expect_true(is.data.frame(result) &&
                ncol(result) == size.expected)
  expect_true(is.numeric(result[, size.expected - 1L]) &&
                is.numeric(result[, size.expected]))
})

test_that("Do log2 transformation value-to-value", {
  result = transformation.apply(
    original.data = data.table::data.table(measure = c(2, 4, 8)),
    columns.to.transform = "measure",
    transformation.functions = "log2"
  )
  expect_true(is.data.frame(result) && ncol(result) == 2L)
  expect_true(is.numeric(result[, 2L]))
  expect_true(all(result[, 2L] == c(1, 2, 3)))
})

test_that("Do rank transformation value-to-value", {
  result = transformation.apply(
    original.data = data.table::data.table(measure = c(2, 4, 1)),
    columns.to.transform = "measure",
    transformation.functions = "rank"
  )
  expect_true(is.data.frame(result) && ncol(result) == 2L)
  expect_true(is.numeric(result[, 2L]))
  expect_true(all(result[, 2L] == c(2, 3, 1)))
})

test_that("Do mean transformation list-to-value", {
  result = transformation.apply(
    original.data = data.table::data.table(measure = c(list(c(
      1, 2, 3
    )), list(c(
      4, 5, 6
    )))),
    columns.to.transform = "measure",
    transformation.functions = "mean"
  )
  expect_true(is.data.frame(result) && ncol(result) == 2L)
  expect_true(is.numeric(result[, 2L]))
  expect_true(all(result[, 2L] == c(2, 5)))
})

test_that("Do log2 transformation list-to-list", {
  result = transformation.apply(
    original.data = data.table::data.table(measure = c(list(c(
      2, 4, 8
    )), list(c(
      16, 32, 64
    )))),
    columns.to.transform = "measure",
    transformation.functions = "log2"
  )
  expect_true(is.data.frame(result) && ncol(result) == 2L)
  expect_true(all(unlist(result[1, 2L]) == c(1, 2, 3)))
  expect_true(all(unlist(result[2, 2L]) == c(4, 5, 6)))
})

test_that("Do rank transformation list-to-list", {
  result = transformation.apply(
    original.data = data.table::data.table(measure = c(list(c(
      2, 4, 1
    )), list(c(
      5, 1, 67
    )))),
    columns.to.transform = "measure",
    transformation.functions = "rank"
  )
  expect_true(is.data.frame(result) && ncol(result) == 2L)
  expect_true(all(unlist(result[1, 2L]) == c(2, 3, 1)))
  expect_true(all(unlist(result[2, 2L]) == c(2, 1, 3)))
})

test_that("Transformation only works with compatible columns", {
  result = transformation.apply(
    original.data = mlr.benchmark.example,
    columns.to.transform = "problem",
    transformation.functions = c("rank", "log2")
  )
  expect_equal(mlr.benchmark.example, result)
})

test_that("Column type detection works properly", {
  data = mlr.benchmark.example
  expect_equal(column.type(data$measure.mmce.test.mean), "values")
  expect_equal(column.type(data$list.mmce), "vector")
  expect_equal(column.type(data$problem), "other")
})

test_that("Numeric column names detection works properly", {
  data = mlr.benchmark.example
  column.names.to.be = list("measure.mmce.test.mean", "measure.ber.test.mean", "measure.timetrain.test.mean", "list.mmce", "list.ber", "list.timetrain")
  column.names.detected = get.num.columns.name(data)
  expect_equal(column.names.detected, column.names.to.be)
})

test_that("Parsing of a list of functions works properly", {
  functions = "mean,sd,rank"
  functions.parsed = parser.function.list(functions)
  expect_equal(functions.parsed, c("mean", "sd", "rank"))
})
