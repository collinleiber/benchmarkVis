context("getBenchMarkUseCase")

test_that("getBenchMarkUseCase result is valid", {
  df = getBenchMarkUseCase()
  expect_true(checkStructure(df))
})
