#' @title get aggregation result
#'
#' @description
#' do aggregation and return a new data table
#'
#' @param fun aggregate functions
#' @param prefix_str the column name's prefix
#' @param groupby the list of columns name that will be grouped
#' @param aggcol the list of columns name that will be aggregated
#' @param df the input dataframe
#' @return a dataframe with aggregated column
get.agg.result = function(fun, prefix_str, groupby, aggcol, df){
  newtable = do.agg(fun, groupby, aggcol, df)
  newtable = newtable[, aggcol, drop = FALSE]
  newcolsname = lapply(aggcol,
                       FUN = function(colname) {
                         newname = paste(prefix_str, "_", colname, "", sep  = "")
                       }
  )
  colnames(newtable) = newcolsname
  return(newtable)
}

#' @title do aggregation result
#'
#' @description
#' do aggregation and return a new data table.
#' the aggcol must include problem and algorithm.
#' only nummeric columns can be aggregated.
#'
#' @param fun aggregate functions
#' @param groupby the list of columns name that will be grouped
#' @param aggcol the list of columns name that will be aggregated
#' @param df the input dataframe
#' @return a dataframe
do.agg = function(fun, groupby, aggcol, df) {
  fun = eval(parse(text = fun))
  tmp = df
  #types = lapply(tmp[, aggcol], class)
  #if(FALSE %in% lapply(types, is.numeric))
  newtable = aggregate(x = tmp[c(aggcol)],
                         by = tmp[c(groupby)],
                         FUN = fun)
  return(newtable)
}

#' @title get aggregation result
#'
#' @description
#' do aggregation and return a new data table.
#' the groupby columns must include problem and algorithm
#' @param groupby the list of columns name that will be grouped
#' @param aggcol the list of columns name that will be aggregated
#' @param aggfun the function to aggregate with
#' @param df the input dataframe
#' @return a dataframe
#' @export
#' @examples
#' aggregation.apply(groupby= c("problem", "algorithm"), aggfun= c("mean"), aggcol= c("measure.mmce.test.mean", "measure.ber.test.mean"), df= mlr.benchmark.example)
aggregation.apply = function(groupby, aggfun, aggcol, df) {
  checkmate::assert_data_frame(df)
  #result = do.agg("mean", groupby, aggcol, df)
  result = df[, groupby, drop = FALSE]
  for (x in aggfun){
    if (check.agg.valid(x)) {
      newtable = get.agg.result(eval(x), x, groupby, aggcol, df)
      result = cbind(result, newtable)
    }
  }
  return(result)
}

#TODO documentation
#' @title get transformation result
#'
#' @description
#' do transformation and return a new data table.
#' the groupby columns must include problem and algorithm
#' @param groupby the list of columns name that will be grouped
#' @param aggcol the list of columns name that will be aggregated
#' @param aggfun the function to aggregate with
#' @param df the input dataframe
#' @return a dataframe
#' @export
#' @examples
#' transformation.apply(original.data = mlr.benchmark.example, columns.to.transform = c("measure.mmce.test.mean", "measure.ber.test.mean"), transformation.functions = c("log2"))
transformation.apply = function(original.data, columns.to.transform, transformation.functions) {
  checkmate::assert_data_frame(original.data)
  result = original.data
  for (transform.func in transformation.functions) { #TODO replace for-loops ?
    if (check.transform.valid(transform.func)) {
      for (column in columns.to.transform){
        transformed.column = unlist(lapply(original.data[, column], transform.func))
        result = cbind(result, transformed.column) #TODO: is it efficient?
      }
    }
  }
  return(result)
}

#TODO: document; test
#rank  = function(data, measure) {
#  data magrittr::%>% dplyr::mutate(rank = order(eval(parse(text = measure))))
#  order.scores = order(data[, x])
#  #rank = NA
#  rank[order.scores] = seq_len(nrow(data))
#  name = paste0("rank(", measure, ")")
#  rank = as.factor(rank)
#  data = cbind(data, rank)
#  colnames(data)[ncol(data)] = name
#  return(data)
#}


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
#' @examples
#' parser.function.list("mean,standard deviation,median")
parser.function.list = function(function.list)
{
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