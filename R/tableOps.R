#' @title get aggregation result
#'
#' @description
#' do aggregation and return a new data table
#'
#' @param fun aggregate functions
#' @param prefix_str the column name's prefix
#' @param groupby the list of columns name that will be grouped
#' @param aggcol the list of columns name that will be aggregated
#' @param dt the input dataframe
#' @return a dataframe with aggregated column
get.agg.result = function(fun, prefix_str, groupby, aggcol, dt) {
  newtable = aggregate(x = dt[aggcol],
                       by = dt[groupby],
                       FUN = eval(parse(text = fun)))
  colnames(newtable) = lapply(
    colnames(newtable),
    FUN = function(colname) {
      if (colname %in% aggcol) {
        newname = paste(colname, "_", prefix_str, "", sep  = "")
      }
      else {
        colname
      }
    }
  )
  return(data.table::as.data.table(newtable))
}

#' @title apply aggregation function
#'
#' @description
#' do aggregation and return a new data table.
#' the groupby columns must include problem and algorithm
#' @param groupby the list of columns name that will be grouped
#' @param aggcol the list of columns name that will be aggregated
#' @param aggfun the function to aggregate with
#' @param dt the input dataframe
#' @return a dataframe
#' @export
#' @examples
#' aggregation.apply(groupby= c("problem", "algorithm"), aggfun= c("mean"), aggcol= c("measure.mmce.test.mean", "measure.ber.test.mean"), dt= mlr.benchmark.example)
aggregation.apply = function(groupby, aggfun, aggcol, dt) {
  checkmate::assert_data_table(dt)
  for (x in aggfun) {
    if (check.aggregation.valid(x)) {
      result = get.agg.result(eval(x), x, groupby, aggcol, dt)
    }
    else {
      result = dt
    }
  }
  return(result)
}

#' @title check if the input function is a valid aggregation function
#'
#' @description
#' check if the input function is a valid aggregation function (i.e., returns a single numeric value)
#' @param agg.fun aggregation function
#' @return boolean
check.aggregation.valid = function(agg.fun) {
  if (is.character(agg.fun))
    agg.fun = eval(parse(text = agg.fun))
  result = do.call(agg.fun, list(c(1, 2, 3, 4)))
  is.numeric(result) && length(result) == 1
}

#' @title apply transformation functions
#'
#' @description
#' do transformation and return a new data table
#' @param original.data a data table in benchmarkVis format
#' @param columns.to.transform the list of columns to apply the functions on
#' @param transformation.functions the functions for transofrmation of data
#' @return a data table containing the original data and transformed versions of columns
#' @export
#' @examples
#' transformation.apply(original.data = mlr.benchmark.example, columns.to.transform = c("measure.mmce.test.mean", "measure.ber.test.mean"), transformation.functions = c("log2"))
transformation.apply = function(original.data,
                                columns.to.transform,
                                transformation.functions) {
  checkmate::assert_data_frame(original.data)
  result = original.data
  for (transform.func in transformation.functions) {
    transform.function = eval.function(transform.func)
    for (column in columns.to.transform) {
      # Measure -> Measure or List -> Measure transformation
      if ((
        column.type(original.data[, column]) == "values" &&
        check.transform.value.to.value(transform.function)
      ) ||
      (
        column.type(original.data[, column]) == "vector" &&
        !is.character(original.data[[column]]) &&
        check.transform.list.to.value(transform.function)
      )) {
        if (transform.func == "rank")
          transformed.column = rank(original.data[, column])
        else
          transformed.column = unlist(lapply(original.data[, column], transform.function))
        column.name = column
        if (grepl("list.", column.name)) {
          column.name = sub("list.", "measure.from.list.", column.name)
        }
        new.column.name = paste(column.name, "_", transform.func, "", sep  = "")
      }
      # List -> List transformation
      else if (column.type(original.data[, column]) == "vector" &&
                !is.character(original.data[[column]]) &&
               check.transform.list.to.list(transform.function)) {
        if (transform.func == "rank")
          transformed.column = lapply(original.data[, column], rank)
        else
          transformed.column = lapply(original.data[, column], function(x) {
            unlist(lapply(x, transform.function))
          })
        new.column.name = paste(column, "_", transform.func, "", sep  = "")
      }
      # Incorrect tranformation
      else {
        transformed.column = NULL
        next
      }
      result$transformed.column = transformed.column
      data.table::setnames(result, "transformed.column", new.column.name)
    }
  }

  return(data.table::as.data.table(result))
}

#' @title check if the input function is a valid value-to-value transformation function
#'
#' @description
#' check if the input function is a valid value-to-value transformation function
#' (i.e., applied to a list of numeric values
#' returns a list of numeric values of the same length)
#' [transformation R^n => R^n]
#' @param transform.fun transformation function
#' @return boolean
check.transform.value.to.value = function(transform.fun) {
  result.num = transform.fun(1)
  is.value = is.numeric(result.num) && length(result.num) == 1

  test.list = c(1, 2, 3, 4)
  result.list = lapply(test.list, transform.fun)
  is.list.of.values = is.list(result.list) &&
    !(FALSE %in% lapply(result.list, is.numeric)) &&
    length(result.list) == length(test.list)

  return(is.value && is.list.of.values)
}

#' @title check if the input function is a valid list-to-value transformation function
#'
#' @description
#' check if the input function is a valid list-to-value transformation function
#' (i.e., applied to a list of lists of numeric values
#' returns a list of numeric values of the same length)
#' [transformation R^n x R^m => R^n]
#' @param transform.fun transformation function
#' @return boolean
check.transform.list.to.value = function(transform.fun) {
  test.list = c(1, 2, 3, 4)
  result.num = transform.fun(test.list)
  is.value = is.numeric(result.num) && length(result.num) == 1

  test.list.column = c(list(c(1, 2, 3, 4)), list(c(1, 2, 3, 4)), list(c(1, 2, 3, 4)), list(c(1, 2, 3, 4)))
  result.list = lapply(test.list.column, transform.fun)
  is.list.of.values = is.list(result.list) &&
    !(FALSE %in% lapply(result.list, is.numeric)) &&
    !(FALSE %in% lapply(result.list, function(x)
      length(x) == 1L)) &&
    length(result.list) == length(test.list)

  return(is.value && is.list.of.values)
}

#' @title check if the input function is a valid list-to-list transformation function
#'
#' @description
#' check if the input function is a valid list-to-list transformation function
#' (i.e., applied to a list of lists of numeric values
#' returns a list of lists of numeric values of the same length)
#' [transformation R^n x R^m => R^n x R^m]
#' @param transform.fun transformation function
#' @return boolean
check.transform.list.to.list = function(transform.fun) {
  test.list = c(1, 2, 3, 4)
  result.list = lapply(test.list, transform.fun)
  is.list.of.values = is.vector(result.list) &&
    length(result.list) == length(test.list) &&
    !(FALSE %in% lapply(result.list, is.numeric))

  test.list.column = c(list(c(1, 2, 3, 4)), list(c(1, 2, 3, 4)), list(c(1, 2, 3, 4)), list(c(1, 2, 3, 4)))
  result.list.of.lists = lapply(test.list.column, function(x) {
    unlist(lapply(x, transform.fun))
  })
  is.list.of.lists = is.vector(result.list.of.lists) &&
    !(FALSE %in% unlist(lapply(result.list.of.lists, function(x) {
      lapply(x, is.numeric)
    }))) &&
    !(FALSE %in% lapply(result.list.of.lists, is.vector)) &&
    !(FALSE %in% lapply(result.list.of.lists, function(x)
      length(x) == 4L)) &&
    !(FALSE %in% lapply(result.list.of.lists, function(x) {
      lapply(x, function(x)
        length(x) == 1L)
    })) &&
    length(result.list.of.lists) == length(test.list.column)

  return(is.list.of.values && is.list.of.lists)
}

#' @title get type of values of a column (numeric values, vector or other)
#'
#' @description
#' check if a data frame's column contains numeric values,
#' vectors or is of other type
#' @param column column of a data frame to check
#' @return string description of values' type
column.type = function(column) {
  if (is.numeric(column))
    return("values")
  if (!(FALSE %in% lapply(column, is.vector)))
    return("vector")
  return("other")
}

#' @title evaluate the input function
#'
#' @description
#' if the input is a string parses and evaluates the function contained
#' @param fun input function (string or function)
#' @return function
eval.function = function(fun) {
  if (is.character(fun)) {
    fun = eval(parse(text = fun))
  }
  return(fun)
}

#' @title get numeric columns' names for a dataframe
#'
#' @description
#' detects numeric columns' names for a dataframe
#' for numeric values as well as lists of numeric values
#'
#' @param data a dataframe
#' @return list of the numeric column names
get.num.columns.name = function(data) {
  colnames = list()
  for (col_name in names(data)) {
    if (is.numeric(data[, col_name])) {
      colnames = c(colnames, col_name)
    }
    else if (is.vector(data[, col_name]) &&
             !(FALSE %in% lapply(data[, col_name], is.numeric))) {
      colnames = c(colnames, col_name)
    }
  }
  return(colnames)
}

#' @title parse the text input of a list of functions
#'
#' @description
#' parse the list of functions to a vector of function names.
#' @param function.list list of functions to parse
#' @return a vector
parser.function.list = function(function.list) {
  function.list = strsplit(function.list, ",")
  function.list = unlist(function.list, use.names = FALSE)
  return(function.list)
}
