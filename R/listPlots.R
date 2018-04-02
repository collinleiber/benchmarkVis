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
#' @param show.histogram shows the histogram of the measure values in the background (default: FALSE)
#' @param color.by defines the color of the line. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @param style.by defines the style of the line. Possibilities: "algorithm", "problem", "replication" (default: "problem")
#' @return a plotly line plot
#' @export
#' @examples
#' createListLinePlot(microbenchmark.example, "list.values", "id", TRUE)
createListLinePlot = function(dt, list.measure, cumulative.function = "id", show.histogram = FALSE, color.by = "algorithm", style.by = "problem") {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(list.measure)
  checkmate::assert_string(cumulative.function)
  checkmate::assert_true(cumulative.function %in% c("id", "max", "min", "mean"))
  checkmate::assert_true(list.measure %in% getLists(dt))
  checkmate::assert_string(color.by)
  checkmate::assert_string(style.by)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  checkmate::assert_true(style.by %in% getMainColumns(dt))
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
    style = rep(dt[[style.by]], rep(max.iterations, nrow(dt))),
    color = rep(dt[[color.by]], rep(max.iterations, nrow(dt)))
  )
  # Get text
  if ("replication" %in% getMainColumns(dt)) {
    new.df$text = paste("iteration: ", new.df$iteration, "<br>problem: ", rep(dt$problem, rep(max.iterations, nrow(dt))), "<br>algorithm: ", rep(dt$algorithm, rep(max.iterations, nrow(dt))), "<br>replication: ", rep(dt$replication, rep(max.iterations, nrow(dt))), sep = "")
  } else {
    new.df$text = paste("iteration: ", new.df$iteration, "<br>problem: ", rep(dt$problem, rep(max.iterations, nrow(dt))), "<br>algorithm: ", rep(dt$algorithm, rep(max.iterations, nrow(dt))), sep = "")
  }
  # Create plot
  p = plotly::plot_ly(new.df)
  p = plotly::add_trace(p, x = ~iteration, y = ~measure, color = ~color, linetype = ~style, type = "scatter", mode = "lines+markers", text = ~text)
  if (show.histogram) {
  p = plotly::add_histogram(p,
    y = ~ measure,
    alpha = 0.4,
    name = "Measure distribution",
    xaxis = "x2")
  p = plotly::layout(
    p,
    yaxis = list(title = list.measure),
    xaxis = list(overlaying = "x2"),
    xaxis2 = list(side = "top", title = "Count"),
    margin = list(t = 130)
  )
  } else {
  p = plotly::layout(p, yaxis = list(title = list.measure))
}
  return(p)
}

#' @title Create a list line plot with a measure on x and y aixs
#'
#' @description
#' Create a plotly line plot out of a benchmarkVis compatible data table.
#' The created line chart shows the change within the specified list measures
#'
#' @param dt campatible data table
#' @param list.measure1 the list measure on the x axis
#' @param list.measure2 the list measure on the y axis
#' @param draw.lines draw a line between the points in the original order (default: FALSE)
#' @param color.by defines the color of the markers. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @param style.by defines the style of the line. Just working with 'draw.lines' = TRUE. Possibilities: "algorithm", "problem", "replication" (default: "problem")
#' @return a plotly line plot
#' @export
#' @examples
#' createListDualMeasurePlot(mlr.benchmark.example, "list.mmce", "list.ber")
createListDualMeasurePlot = function(dt, list.measure1, list.measure2, draw.lines = FALSE, color.by = "algorithm", style.by = "problem") {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(list.measure1)
  checkmate::assert_string(list.measure2)
  checkmate::assert_true(list.measure1 %in% getLists(dt))
  checkmate::assert_true(list.measure2 %in% getLists(dt))
  checkmate::assert_string(color.by)
  checkmate::assert_string(style.by)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  checkmate::assert_true(style.by %in% getMainColumns(dt))
  # Get maximum amount of lists
  times = sapply(dt[[list.measure1]], function(x) {
    return(length(x))
  })
  # Create new plotly compatible data table
  new.df = data.frame(
    measure1 = unlist(dt[[list.measure1]]),
    measure2 = unlist(dt[[list.measure2]]),
    style = rep(dt[[style.by]], times),
    color = rep(dt[[color.by]], times),
    iteration = unlist(lapply(times, seq))
  )
  # Get text
  if ("replication" %in% getMainColumns(dt)) {
    new.df$text = paste("iteration: ", new.df$iteration, "<br>problem: ", rep(dt$problem, times), "<br>algorithm: ", rep(dt$algorithm, times), "<br>replication: ", rep(dt$replication, times), sep = "")
  } else {
    new.df$text = paste("iteration: ", new.df$iteration, "<br>problem: ", rep(dt$problem, times), "<br>algorithm: ", rep(dt$algorithm, times), sep = "")
  }
  # Create plot
  if (draw.lines) {
    p = plotly::plot_ly(new.df, x = ~measure1, y = ~measure2, color = ~color, linetype = ~style, text = ~text, type = "scatter", mode = "lines+markers")
  } else {
    p = plotly::plot_ly(new.df, x = ~measure1, y = ~measure2, color = ~color, text = ~text, type = "scatter", mode = "markers")
  }
  p = plotly::layout(p, xaxis = list(title = list.measure1), yaxis = list(title = list.measure2))
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
  new.df = data.table::data.table(rank = as.factor(rep(seq(nrow(
    dt
  )), nrow(dt))))
  if ("replication" %in% getMainColumns(dt)) {
    new.df$inter = rep(interaction(dt$problem, dt$algorithm, dt$replication),
      rep(nrow(dt), nrow(dt)))
  } else {
    new.df$inter = rep(interaction(dt$problem, dt$algorithm),
      rep(nrow(dt), nrow(dt)))
  }
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
  # Get text
  if ("replication" %in% getMainColumns(dt)) {
    new.df$text = paste("rank: ", new.df$rank, "<br>count: ", new.df$count, "<br>problem: ", rep(dt$problem, rep(nrow(dt), nrow(dt))), "<br>algorithm: ", rep(dt$algorithm, rep(nrow(dt), nrow(dt))), "<br>replication: ", rep(dt$replication, rep(nrow(dt), nrow(dt))), sep = "")
  } else {
    new.df$text = paste("rank: ", new.df$rank, "<br>count: ", new.df$count, "<br>problem: ", rep(dt$problem, rep(nrow(dt), nrow(dt))), "<br>algorithm: ", rep(dt$algorithm, rep(nrow(dt), nrow(dt))), sep = "")
  }
  # Create plot
  p = plotly::plot_ly(
    new.df,
    x = ~ rank,
    y = ~ count,
    type = "bar",
    color = ~ inter,
    text = ~ text,
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
  # Give each value of the list measure its own row
  combined.df = data.frame()
  for (row in seq(nrow(dt))) {
    list.length = length(dt[[row, list.measure]])
    df = data.frame(measure = dt[[row, list.measure]])
    if ("replication" %in% getMainColumns(dt)) {
      df$entry = paste(rep(dt[[row, "problem"]], list.length),
        rep(dt[[row, "algorithm"]], list.length),
        rep(dt[[row, "replication"]], list.length), sep = ".")
    } else {
      df$entry = paste(rep(dt[[row, "problem"]], list.length),
        rep(dt[[row, "algorithm"]], list.length), sep = ".")
    }
    combined.df = rbind(combined.df, df)
  }
  # Create plot
  p = ggplot2::ggplot(data = combined.df, ggplot2::aes(measure, fill = entry)) + ggplot2::theme_bw()
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
      measure = unlist(rank.measure[[row]])
    )
    if ("replication" %in% getMainColumns(dt)) {
      df$entry = paste(rep(dt[[row, "problem"]], min.iterations),
        rep(dt[[row, "algorithm"]], min.iterations),
        rep(dt[[row, "replication"]], min.iterations), sep = ".")
    } else {
      df$entry = paste(rep(dt[[row, "problem"]], min.iterations),
        rep(dt[[row, "algorithm"]], min.iterations), sep = ".")
    }
    combined.df = rbind(combined.df, df)
  }
  # Create plot
  p = ggplot2::ggplot(data = combined.df, ggplot2::aes(measure, fill = entry))  + ggplot2::theme_bw()
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
