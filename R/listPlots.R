#' @title Create a list line plot
#'
#' @description
#' Create a plotly line plot out of a benchmarkVis compatible data table.
#' The created line chart shows the change within the specified list measure for each iteration
#' A cumulative function can be defined to get a better feeling of the course of the line chart.
#' Possible parameters for the cumulative function are: "id" (identity function), "min", "max", "mean" - default is id.
#'
#' @param dt campatible data table
#' @param list.measure the column name containing the list of a specific measure
#' @param cumulative.function the cumulative function to use for the list measure (default: "id")
#' @return a plotly line plot
#' @export
#' @examples
#' createListLinePlot(microbenchmark.example, "list.values")
createListLinePlot = function(dt, list.measure, cumulative.function = "id") {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(list.measure)
  checkmate::assert_string(cumulative.function)
  checkmate::assert_true(cumulative.function %in% c("id", "max", "min", "mean"))
  checkmate::assert_true(list.measure %in% getLists(dt))
  # Get maximum amount of lists
  max.iterations = max(sapply(dt[[list.measure]], function(x) {
    return(length(x))
  }))
  # Add NaN to fill problems with less iterations
  replications = lapply(dt[[list.measure]], function(x) {
    return(c(x, rep(NaN, max.iterations - length(x))))
  })
  # Use cumulative function
  replications = lapply(replications, function(x) {
    getCumulativeValues(x, cumulative.function)
  })
  # Create new plotly compatible data table
  new.df = data.frame(
    iteration = rep(1:max.iterations, nrow(dt)),
    measure = unlist(replications),
    problem = rep(dt$problem, rep(max.iterations, nrow(dt))),
    algorithm = rep(dt$algorithm, rep(max.iterations, nrow(dt)))
  )
  # Create plot
  p = plotly::plot_ly(new.df, x = ~iteration, y = ~measure, color = ~algorithm, linetype = ~problem, type = "scatter", mode = "lines+markers")
  # Convert plot to plotly
  p = plotly::layout(p, yaxis = list(title = list.measure))
  return(p)
}

#' @title Create a list rank matrix bar plot
#'
#' @description
#' Create a plotly rank matrix bar plot out of a benchmarkVis compatible data table.
#' The created bar chart shows the the rank of the specified list measure for each iteration.
#'
#' @param dt campatible data table
#' @param list.measure the column name containing the list of a specific measure
#' @return a plotly rank matrix bar plot
#' @export
#' @examples
#' createListRankMatrixBarPlot(microbenchmark.example, "list.values")
createListRankMatrixBarPlot = function(dt, list.measure) {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(list.measure)
  checkmate::assert_true(list.measure %in% getLists(dt))
  # Get minimum amount of lists
  min.iterations = min(sapply(dt[[list.measure]], function(x) {
    return(length(x))
  }))
  # Remove values if there are more than min.iterations
  lists = lapply(dt[[list.measure]], function(x) {
    return(x[1:min.iterations])
  })
  # Add ranks
  rank.measure = repList(list(), nrow(dt))
  for (i in 1:min.iterations) {
    tmp = vector()
    for (row in seq(nrow(dt))) {
      tmp[row] = lists[[row]][i]
    }
    # Get ranks
    for (row in seq(nrow(dt))) {
      rank.measure[[row]][i] = rank(tmp, ties.method = "min")[row]
    }
  }
  # Create new dataframe
  new.df = data.table::data.table(algorithm = rep(interaction(dt$problem, dt$algorithm),
    rep(nrow(dt), nrow(dt))),
    rank = as.factor(rep(seq(nrow(
      dt
    )), nrow(dt))))
  # Count ranks
  counter = vector()
  for (row in seq(nrow(dt))) {
    # Go through all possible ranks
    for (i in seq(nrow(dt))) {
      count = 0
      for (rank in rank.measure[[row]]) {
        # Count matches
        if (rank == i) {
          count = count + 1
        }
      }
      counter = c(counter, count)
    }
  }
  new.df$count = counter
  # Aggregate
  #new.df = new.df[, .( count = sum(count)), by = .(algorithm, rank)]
  # Create plot
  p = plotly::plot_ly(
    new.df,
    x = ~ rank,
    y = ~ count,
    type = "bar",
    color = ~ algorithm,
    marker = list(line = list(color = "gray", width = 2))
  )
  p = plotly::layout(
    p,
    yaxis = list(title = "frequency"),
    xaxis = list(title = "ranks"),
    barmode = "stack"
  )
  return(p)
}

#' @title Create a list density plot
#'
#' @description
#' Create a plotly density plot out of a benchmarkVis compatible data table.
#' The created density chart shows the distribution of the specified list measure
#'
#' @param dt campatible data table
#' @param list.measure the column name containing the list of a specific measure
#' @param stack.plots defines if the density curves should be stacked. Alternative is transparent. Default: FALSE
#' @return a plotly density plot
#' @export
#' @examples
#' createListDensityPlot(microbenchmark.example, "list.values", TRUE)
createListDensityPlot = function(dt, list.measure, stack.plots = FALSE) {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(list.measure)
  checkmate::assert_logical(stack.plots)
  checkmate::assert_true(list.measure %in% getLists(dt))
  combined.df = data.frame()
  for (row in seq(nrow(dt))) {
    list.length = length(dt[[row, list.measure]])
    df = data.frame(problem.algorithm = paste(rep(dt[[row, "problem"]], list.length),
      rep(dt[[row, "algorithm"]], list.length), sep = "."),
      measure = dt[[row, list.measure]])
    combined.df = rbind(combined.df, df)
  }
  # Create plot
  p = ggplot2::ggplot(data = combined.df, ggplot2::aes(measure, fill = problem.algorithm))  + ggplot2::theme_bw()
  if (stack.plots) {
    p = p + ggplot2::geom_density(position = "stack")
  } else {
    p = p + ggplot2::geom_density(alpha = 0.4)
  }
  # Convert plot to plotly
  p = plotly::ggplotly(p)
  p = plotly::layout(p, xaxis = list(title = list.measure))
  return(p)
}

#' @title Create a list density rank plot
#'
#' @description
#' Create a plotly density plot out of a benchmarkVis compatible data table.
#' The created density chart shows the distribution of the specified list measure by its ranks.
#'
#' @param dt campatible data table
#' @param list.measure the column name containing the list of a specific measure
#' @param stack.plots defines if the density curves should be stacked. Alternative is transparent. Default: FALSE
#' @return a plotly density plot
#' @export
#' @examples
#' createListDensityRankPlot(microbenchmark.example, "list.values", TRUE)
createListDensityRankPlot = function(dt, list.measure, stack.plots = FALSE) {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(list.measure)
  checkmate::assert_logical(stack.plots)
  checkmate::assert_true(list.measure %in% getLists(dt))
  # Get minimum amount of lists
  min.iterations = min(sapply(dt[[list.measure]], function(x) {
    return(length(x))
  }))
  # Remove values if there are more than min.iterations
  lists = lapply(dt[[list.measure]], function(x) {
    return(x[1:min.iterations])
  })
  # Add ranks
  rank.measure = repList(list(), nrow(dt))
  for (i in 1:min.iterations) {
    tmp = vector()
    for (row in seq(nrow(dt))) {
      tmp[row] = lists[[row]][i]
    }
    # Get ranks
    for (row in seq(nrow(dt))) {
      rank.measure[[row]][i] = rank(tmp, ties.method = "min")[row]
    }
  }
  # Create new dataframe
  combined.df = data.frame()
  for (row in seq(nrow(dt))) {
    df = data.frame(
      problem.algorithm = paste(rep(dt[[row, "problem"]], min.iterations),
        rep(dt[[row, "algorithm"]], min.iterations), sep = "."),
      measure = unlist(rank.measure[[row]])
    )
    combined.df = rbind(combined.df, df)
  }
  # Create plot
  p = ggplot2::ggplot(data = combined.df, ggplot2::aes(measure, fill = problem.algorithm))  + ggplot2::theme_bw()
  if (stack.plots) {
    p = p + ggplot2::geom_density(position = "stack")
  } else {
    p = p + ggplot2::geom_density(alpha = 0.4)
  }
  # Convert plot to plotly
  p = plotly::ggplotly(p)
  p = plotly::layout(p, xaxis = list(title = "ranks"))
  return(p)
}
