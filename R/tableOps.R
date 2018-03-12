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
get.agg.result = function(fun, prefix_str, groupby, aggcol, dt){
  newtable = aggregate(x = dt[aggcol],
                         by = dt[groupby],
                         FUN = eval(parse(text = fun)))
  colnames(newtable) = lapply(colnames(newtable),
                       FUN = function(colname) {
                         if (colname %in% aggcol) {
                            newname = paste(prefix_str, "_", colname, "", sep  = "")
                         }
                         else {
                           colname
                         }
                       }
  )
  return(newtable)
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
  for (x in aggfun){
    if (check.agg.valid(x)) {
      result = get.agg.result(eval(x), x, groupby, aggcol, dt)
    }
  }
  return(result)
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
transformation.apply = function(original.data, columns.to.transform, transformation.functions) {
  checkmate::assert_data_frame(original.data)
  result = original.data
  for (transform.func in transformation.functions) { #TODO replace for-loops ?
      if (check.transform.valid(transform.func)) {
        for (column in columns.to.transform){
          if (transform.func == "rank") {
            transformed.column = rank(original.data[, column])
          }
          else {
            transformed.column = unlist(lapply(original.data[, column], transform.func))
          }
          result = cbind(result, transformed.column) #TODO: is it efficient?
          new.column.name = paste(transform.func, "_", column, "", sep  = "")
          data.table::setnames(result, "transformed.column", new.column.name)
      }
    }
  }
  return(result)
}



#' @title do aggregation result
#'
#' @description
#' do aggregation and return a new data table.
#' the aggcol must include problem and algorithm
#'
#' @param data a dataframe
#' @return list of the nummeric column names
get.num.columns.name = function(data) {
  colnames = list()
  for (col_name in names(data))
  {
    if (is.numeric(data[, col_name]))
    {
      colnames = c(colnames, col_name)
    }
  }
  colnames
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

#' @title check if the input function is a valid aggregation function
#'
#' @description
#' check if the input function is a valid aggregation function (i.e., returns a single numeric value)
#' @param agg.fun aggregation function
#' @return boolean
check.agg.valid = function(agg.fun) {
  if (is.character(agg.fun)) agg.fun = eval(parse(text = agg.fun))
  result = do.call(agg.fun, list(c(1, 2, 3, 4)))
  is.numeric(result) && length(result) == 1
}

#' @title check if the input function is a valid transformation function
#'
#' @description
#' check if the input function is a valid transformation function (i.e., applied to a list
#' of numeric values returns a list of numeric values of the same length)
#' @param transform.fun transformation function
#' @return boolean
check.transform.valid = function(transform.fun) {
  if (is.character(transform.fun)) transform.fun = eval(parse(text = transform.fun))
  result.num = transform.fun(1)
  result.list = lapply(c(1, 2, 3, 4), transform.fun)
  is.numeric(result.num) && length(result.num) == 1 && is.list(result.list) && length(result.list) == 4
}
