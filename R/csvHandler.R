#' @title Import a csv file as a benchmarkVis data frame
#'
#' @description
#' Import a matching csv file as a benchmarkVis compatible data frame.
#' Check the file "data/ml.benchmark.csv" to see an example of the correct file structure
#'
#' @param input.csv path to the input csv file
#' @return a dataframe with the benchmarkVis specific structure
#' @export
csvImport = function(input.csv) {
  # Load csv file
  df = read.csv(input.csv)
  # Parse the columns containing list strings back to lists
  df$problem.parameter = lapply(df$problem.parameter, stringToList)
  df$algorithm.parameter = lapply(df$algorithm.parameter, stringToList)
  df$replication.parameter = lapply(df$replication.parameter, stringToList)
  # Change replication list strings back to lists
  for (i in 1:getMeasureCount(df)) {
    column.name = names(df)[ncol(df) - (i - 1)]
    df[[column.name]] = lapply(df[, ncol(df) - (i - 1)], stringToList)
  }
  return(df)
}

#' @title Export a benchmarkVis data frame as csv file
#'
#' @description
#' Export the specified benchmarkVis compatible data frame as an csv file. Lists will be converted into a String:
#' "list(valA = abc, valB = xyz)"
#'
#' @param df the benchmarkVis data frame
#' @param file.path path to save the file to
#' @export
#' @examples
#' data("ml.example")
#' csvExport(ml.example, "test.csv")
csvExport = function(df, file.path) {
  # Create copy of the original dataframe
  df.copy = cbind(df)
  # Parse the columns containing lists to string
  df.copy$problem.parameter = sapply(df$problem.parameter, listToString)
  df.copy$algorithm.parameter = sapply(df$algorithm.parameter, listToString)
  df.copy$replication.parameter = sapply(df$replication.parameter, listToString)
  # Change replication lists to string
  for (i in 1:getMeasureCount(df)) {
    column.name = names(df)[ncol(df) - (i - 1)]
    df.copy[[column.name]] = sapply(df[[column.name]], function(x)
      paste("c(", toString(x), ")", sep = ""))
  }
  # Write to csv file
  write.csv(df.copy, file = file.path, row.names = FALSE)
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
