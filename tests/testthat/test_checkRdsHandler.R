context("Rds Handler")

# Check if exporting and importing an .rds file works properly
test_that("rds Export/Import Test", {
  rdsExport(mlr.benchmark.example, "test.rds")
  dt = rdsImport("test.rds")
  file.remove("test.rds")
  dt2 = cbind(mlr.benchmark.example)
  expect_equal(dt, dt2)
})
