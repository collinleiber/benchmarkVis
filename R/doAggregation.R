#' @title get aggregation result
#'
#' @description
#' do aggregation and return a new data table
#'
#' @param fun_str the string of the aggregate functions
#' @param groupby the list of columns name that will be grouped
#' @param aggcol the list of columns name that will be aggregated
#' @param df the input dataframe
#' @return a dataframe
get.agg.result = function(fun_str, groupby, aggcol, df){
  newtable = do.agg(fun_str, groupby, aggcol, df)
  newtable = newtable[, aggcol, drop = FALSE]
  newcolsname = lapply(aggcol,
                       FUN = function(colname) {
                         newname = paste(fun_str, "_", colname, "", sep  = "")
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
#' @param fun_str the string of the aggregate functions
#' @param groupby the list of columns name that will be grouped
#' @param aggcol the list of columns name that will be aggregated
#' @param df the input dataframe
#' @return a dataframe
do.agg = function(fun_str, groupby, aggcol, df) {
  tmp = df
  tag = TRUE
  check.data.type = function(type){
    tag == tag && type == "numeric"
  }
  types = lapply(tmp[, aggcol], class)
  lapply(types, check.data.type)
  if (tag) {
    if (fun_str == "sd") {
      func = stats::sd
    } else if (fun_str == "mean") {
      func = mean
    } else if (fun_str == "median") {
      func = median
    } else {
      #handle err
    }
    newtable = aggregate(x = tmp[c(aggcol)],
                          by = tmp[c(groupby)],
                          FUN = func)
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
#' get.result(groupby= c("problem", "algorithm"), aggfun= c("mean"), aggcol= c("measure.mmce.test.mean", "measure.ber.test.mean"), df= mlr.benchmark.example)
get.result = function(groupby, aggfun, aggcol, df) {
  checkmate::assert_data_frame(df)
  result = do.agg("mean", groupby, aggcol, df)
  result = result[, groupby, drop = FALSE]
  if (is.element("mean", aggfun)) {
    newtable = get.agg.result("mean", groupby, aggcol, df)
    result = cbind(result, newtable)
  }
  if (is.element("standard deviation", aggfun)) {
    newtable = get.agg.result("sd", groupby, aggcol, df)
    result = cbind(result, newtable)
  }
  if (is.element("median", aggfun)) {
    newtable = get.agg.result("median", groupby, aggcol, df)
    result = cbind(result, newtable)
  }
  return(result)
}

get.new.name = function(aggfun, aggcol)
{
  newcolsname = list()
  if (is.element("mean", aggfun)) {
    tmp.name = lapply(
      aggcol,
      FUN = function(colname) {
        newname = paste("mean", "_", colname, "", sep  = "")
      }
    )
    newcolsname = append(newcolsname, unlist(tmp.name))
  }
  if (is.element("median", aggfun)) {
    tmp.name = lapply(
      aggcol,
      FUN = function(colname) {
        newname = paste("median", "_", colname, "", sep  = "")
      }
    )
    newcolsname = append(newcolsname, tmp.name)
  }
  if (is.element("standard deviation", aggfun)) {
    tmp.name = lapply(
      aggcol,
      FUN = function(colname) {
        newname = paste("sd", "_", colname, "", sep  = "")
      }
    )
    newcolsname = append(newcolsname, tmp.name)
  }
  newcolsname = c(do.call("cbind", newcolsname))
  return(newcolsname)
}
