context("Transformation")

test_that("transformation function is valid", {
  test.fun.1 = function(x) { x * 2 }
  test.fun.2 = function(x) 1 #if x is a list then returns a list
  test.fun.3 = function(x) "test"
  transform.fun.list = list(log, test.fun.1, test.fun.2)
  no.transform.fun.list = list(test.fun.3)
  for (fun in transform.fun.list){
    expect_true(check.transform.valid(fun))
  }
  for (fun in no.transform.fun.list){
    expect_false(check.transform.valid(fun))
  }
})


# Check if Do transformation for mlr benchmark with mean as transformfun works correctly
test_that("Do rank and log2 transformation for mlr benchmark", {
  result = transformation.apply(
    original.data = mlr.benchmark.example,
    columns.to.transform = c("measure.mmce.test.mean", "measure.ber.test.mean"),
    transformation.functions = c("rank", "log2")
  )
  size.expected = ncol(mlr.benchmark.example) + 4L
  expect_true(is.data.frame(result) && ncol(result) == size.expected)
  expect_true(is.numeric(result[, size.expected - 1L]) && is.numeric(result[, size.expected]))
})

test_that("Do log2 transformation", {
  result = transformation.apply(
    original.data = data.frame(measure = c(2, 4, 8)),
    columns.to.transform = "measure",
    transformation.functions = "log2"
  )
  expect_true(is.data.frame(result) && ncol(result) == 2L)
  expect_true(is.numeric(result[, 2L]))
  expect_true(!(FALSE %in% (result[, 2L] == c(1, 2, 3))))
})
