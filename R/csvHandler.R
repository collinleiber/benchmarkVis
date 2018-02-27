#' @title Import a csv file as a benchmarkVis data table
#'
#' @description
#' Import a matching csv file as a benchmarkVis compatible data table
#' Check the file "data/ml.benchmark.csv" to see an example of the correct file structure
#'
#' @param input.csv path to the input csv file
#' @return a data table with the benchmarkVis specific structure
#' @importFrom utils read.csv write.csv
#' @export
csvImport = function(input.csv) {
  # Load csv file
  dt = data.table::fread(input.csv, stringsAsFactors = TRUE, data.table = TRUE)
  checkmate::assert_true(checkColumnNames(dt))
  # Parse the columns containing list strings back to lists
  dt$problem.parameter = lapply(dt$problem.parameter, stringToList)
  dt$algorithm.parameter = lapply(dt$algorithm.parameter, stringToList)
  dt$replication.parameter = lapply(dt$replication.parameter, stringToList)
  # Change vector strings back to vectors
  for (column.name in getLists(dt)) {
    dt[[column.name]] = lapply(dt[[column.name]], stringToList)
  }
  # Check structure
  checkmate::assert_true(checkStructure(dt))
  # Return created data table
  return(dt)
}

#' @title Export a benchmarkVis data table as csv file
#'
#' @description
#' Export the specified benchmarkVis compatible data table as an csv file. Lists will be converted into a String:
#' "list(valA = abc, valB = xyz)"
#'
#' @param dt the benchmarkVis data table
#' @param file.path path to save the file to
#' @export
#' @examples
#' csvExport(mlr.benchmark.example, "test.csv")
csvExport = function(dt, file.path) {
  checkmate::assert_true(checkStructure(dt))
  # Create copy of the original data table
  dt.copy = cbind(dt)
  # Parse the columns containing lists to string
  dt.copy$problem.parameter = sapply(dt$problem.parameter, listToString)
  dt.copy$algorithm.parameter = sapply(dt$algorithm.parameter, listToString)
  dt.copy$replication.parameter = sapply(dt$replication.parameter, listToString)
  # Change vectors to string
  for (column.name in getLists(dt)) {
    dt.copy[[column.name]] = sapply(dt[[column.name]], function(x)
      paste("c(", toString(x), ")", sep = ""))
  }
  # Write to csv file
  data.table::fwrite(dt.copy, file = file.path, row.names = FALSE)
}

# Little helper to convert list to string
listToString = function(x) {
  # Check if list is empty
  if (!length(x)) {
    return("list()")
  }
  # Check weather value is factor or character. If true add quotes
  values = sapply(x, function(y)
    if (is.factor(y) || is.character(y)) {
      paste("'", as.character(y), "'", sep = "")
    } else {
      as.character(y)
    })
  # Create "key = value" pair
  key.value = paste(names(x), values, sep = " = ")
  return(paste("list(", toString(key.value), ")", sep = ""))
}

#Little helper to convert string to list/vector
stringToList = function(x) {
  # Parse object back to original representation
  return(eval(parse(text = as.character(x))))
}
