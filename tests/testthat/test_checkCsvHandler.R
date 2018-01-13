# Check if exporting and importing a csv file works properly
test_that("CSV export and import is consisting", {
  # Load benchmarkVis specific df
  data("ml.example")
  # Export and import csv
  csvExport(ml.example, "test.csv")
  df = csvImport("test.csv")
  file.remove("test.csv")
  identical(df, ml.example)
})
