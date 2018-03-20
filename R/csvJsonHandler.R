#' @title Import a csv file as a benchmarkVis data table
#'
#' @description
#' Import a matching csv file as a benchmarkVis compatible data table.
#' Lists must be a String with following structure:
#' "list(valA = 123, valB = 'xyz')".
#' Vectors must be a String with following structure:
#' "c(1, 2, 3, 4)"
#' Check the file "extdata/ml.benchmark.csv" to see an example of the correct file structure
#'
#' @param input.csv path to the input csv file
#' @return a data table with the benchmarkVis specific structure
#' @export
csvImport = function(input.csv) {
  checkmate::assert_true(endsWith(input.csv, ".csv"))
  # Load csv file
  dt = data.table::fread(input.csv,
    stringsAsFactors = TRUE,
    data.table = TRUE)
  # Transform data table
  dt = tableTransformationImport(dt, TRUE)
  # Return created data table
  return(dt)
}

#' @title Export a benchmarkVis data table as csv file
#'
#' @description
#' Export the specified benchmarkVis compatible data table as an csv file. Lists will be converted into a String:
#' "list(valA = 123, valB = 'xyz')".
#' Vectors will be converted into a String:
#' "c(1, 2, 3, 4)"
#'
#' @param dt the benchmarkVis data table
#' @param file.path path to save the file to
#' @export
#' @examples
#' csvExport(mlr.benchmark.example, "test.csv")
csvExport = function(dt, file.path) {
  checkmate::assert_true(endsWith(file.path, ".csv"))
  checkmate::assert_true(checkStructure(dt))
  # Tranform data table
  dt.copy = tableTransformationExport(dt, TRUE)
  # Write to csv file
  data.table::fwrite(dt.copy, file = file.path, row.names = FALSE)
}

#' @title Import a json file as a benchmarkVis data table
#'
#' @description
#' Import a matching json file as a benchmarkVis compatible data table.
#' Lists must be a String with following structure:
#' "list(valA = 123, valB = 'xyz')".
#' Check the file "extdata/ml.benchmark2.json" to see an example of the correct file structure
#'
#' @param input.json path to the input json file
#' @return a data table with the benchmarkVis specific structure
#' @export
jsonImport = function(input.json) {
  checkmate::assert_true(endsWith(input.json, ".json"))
  # Load json file
  json = jsonlite::read_json(input.json, simplifyVector = TRUE)
  # Create data table
  dt = data.table::data.table(json, stringsAsFactors = TRUE)
  # Transform data table
  dt = tableTransformationImport(dt, FALSE)
  # Return created data table
  return(dt)
}

#' @title Export a benchmarkVis data table as json file
#'
#' @description
#' Export the specified benchmarkVis compatible data table as an json file. Lists will be converted into a String:
#' "list(valA = 123, valB = 'xyz')".
#'
#' @param dt the benchmarkVis data table
#' @param file.path path to save the file to
#' @export
#' @examples
#' jsonExport(mlr.benchmark.example, "test.json")
jsonExport = function(dt, file.path) {
  checkmate::assert_true(endsWith(file.path, ".json"))
  checkmate::assert_true(checkStructure(dt))
  # Transform data table
  dt.copy = tableTransformationExport(dt, FALSE)
  # Write to json file
  json = jsonlite::write_json(
    dt.copy,
    path = file.path,
    pretty = TRUE,
    factor = "string",
    digits = NA
  )
}

# Do all the table tranformations needed for file import
# For csv & json: Transform parameter lists
# For csv only: Transform vectors
tableTransformationImport = function(dt, is.csv) {
  checkmate::assert_true(checkColumnNames(dt))
  # Parse the columns containing list strings back to lists
  dt$problem.parameter = lapply(dt$problem.parameter, stringToList)
  dt$algorithm.parameter = lapply(dt$algorithm.parameter, stringToList)
  dt$replication.parameter = lapply(dt$replication.parameter, stringToList)
  # Just for CSV files
  if (is.csv) {
    # Change vector strings back to vectors
    for (column.name in getLists(dt)) {
      dt[[column.name]] = lapply(dt[[column.name]], stringToList)
    }
  }
  # Check structure
  checkmate::assert_true(checkStructure(dt))
  # Return transformes data table
  return(dt)
}

# Do all the table tranformations needed for file export.
# For csv & json: Transform parameter lists
# For csv only: Transform vectors
tableTransformationExport = function(dt, is.csv) {
  # Create copy of the original data table
  dt.copy = cbind(dt)
  # Parse the columns containing lists to string
  dt.copy$problem.parameter = sapply(dt$problem.parameter, listToString)
  dt.copy$algorithm.parameter = sapply(dt$algorithm.parameter, listToString)
  dt.copy$replication.parameter = sapply(dt$replication.parameter, listToString)
  # Just for CSV files
  if (is.csv) {
    # Change vectors to string
    for (column.name in getLists(dt)) {
      dt.copy[[column.name]] = sapply(dt[[column.name]], function(x)
        paste("c(", toString(x), ")", sep = ""))
    }
  }
  # Return transformes data table copy
  return(dt.copy)
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
