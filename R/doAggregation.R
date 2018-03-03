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
#' @return a dataframe
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
  tmp = df
  tag = TRUE
  check.data.type = function(type){
    tag == tag && type == "numeric"
  }
  types = lapply(tmp[, aggcol], class)
  lapply(types, check.data.type)
  if (tag) {
    newtable = aggregate(x = tmp[c(aggcol)],
                         by = tmp[c(groupby)],
                         FUN = fun)
    return(newtable)
  }else {
    #   #err msg
    warning("invaild params to aggregate")
    showNotification("invaild params to aggregate", type = "error")
    #validate(need(!tag, "invaild type to aggregate."))
    tmp
  }
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


#' @title parse the text input of agg function.
#'
#' @description
#' parse the text include agg function to a vector of function names.
#' @param aggfun the text include agg function.
#' @return a vector
#' @export
#' @examples
#' parser.agg.input("mean,standard deviation,median")
parser.agg.input <- function(aggfun)
{
  aggfun = strsplit(aggfun, ",")
  aggfun = unlist(aggfun, use.names=FALSE)
  return(aggfun)
}

#' @title check the input is a valid aggragate function name
#'
#' @description
#' check the input is a valid aggragate function name
#' @param x the string of the aggragate function.
#' @return bool
#' @export
#' @examples
#' check.agg.valid("mean")
check.agg.valid = function(x)
{
  #http://www.r-tutor.com/elementary-statistics/numerical-measures
  aggfun_list <- list("max", "min", "rank", "mean", "sd", "median", "quantile", "range", "IQR", "var")
  tag = is.element(x, aggfun_list)
  return(tag)
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
#' get.result(groupby= c("problem", "algorithm"), aggfun= c("mean"), aggcol= c("mmce.test.mean", "ber.test.mean"), df= mlr.benchmark.example)
get.result = function(groupby, aggfun, aggcol, df) {
  checkmate::assert_data_frame(df)
  result = do.agg("mean", groupby, aggcol, df)
  result = result[, groupby, drop = FALSE]
  for(x in aggfun){
    tag = TRUE
    tag = check.agg.valid(x)
    if (tag) {
      newtable = get.agg.result(eval(x), x, groupby, aggcol, df)
      result = cbind(result, newtable)
    }
  }
  return(result)
}

