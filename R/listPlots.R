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
#' @param regression.line add a regression line to the plot (default: FALSE)
#' @return a plotly line plot
#' @export
#' @examples
#' createListDualMeasurePlot(mlr.benchmark.example, "list.mmce", "list.ber")
createListDualMeasurePlot = function(dt, list.measure1, list.measure2, draw.lines = FALSE, color.by = "algorithm", style.by = "problem", regression.line = FALSE) {
  checkmate::assert_data_table(dt)
  checkmate::assert_logical(regression.line)
  checkmate::assert_logical(draw.lines)
  checkmate::assert_string(list.measure1)
  checkmate::assert_string(list.measure2)
  checkmate::assert_true(list.measure1 %in% getLists(dt))
  checkmate::assert_true(list.measure2 %in% getLists(dt))
  checkmate::assert_string(color.by)
  checkmate::assert_string(style.by)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  checkmate::assert_true(style.by %in% getMainColumns(dt))
  # Get length of lists
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
  p = plotly::plot_ly(new.df)
  if (draw.lines) {
    p = plotly::add_trace(p, x = ~measure1, y = ~measure2, color = ~color, linetype = ~style, text = ~text, type = "scatter", mode = "lines+markers")
  } else {
    p = plotly::add_trace(p, x = ~measure1, y = ~measure2, color = ~color, text = ~text, type = "scatter", mode = "markers")
  }
  # Add regression line
  if (regression.line) {
    fit = lm(measure2 ~ measure1, data = new.df)
    p = plotly::add_lines(p, x = ~measure1, y = fitted(fit), name = "regression")
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
#' @param stacked defines if the bars should be stacked (default: TRUE)
#' @return a plotly rank matrix bar plot
#' @export
#' @examples
#' createListRankMatrixBarPlot(microbenchmark.example, "list.values")
createListRankMatrixBarPlot = function(dt, list.measure, stacked = TRUE) {
  checkmate::assert_data_table(dt)
  checkmate::assert_logical(stacked)
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
  if ("replication" %in% getMainColumns(dt)) {
    new.df$inter = rep(interaction(dt$problem, dt$algorithm, dt$replication),
      rep(nrow(dt), nrow(dt)))
    new.df$text = paste("rank: ", new.df$rank, "<br>count: ", new.df$count, "<br>problem: ", rep(dt$problem, rep(nrow(dt), nrow(dt))), "<br>algorithm: ", rep(dt$algorithm, rep(nrow(dt), nrow(dt))), "<br>replication: ", rep(dt$replication, rep(nrow(dt), nrow(dt))), sep = "")
  } else {
    new.df$text = paste("rank: ", new.df$rank, "<br>count: ", new.df$count, "<br>problem: ", rep(dt$problem, rep(nrow(dt), nrow(dt))), "<br>algorithm: ", rep(dt$algorithm, rep(nrow(dt), nrow(dt))), sep = "")
    new.df$inter = rep(interaction(dt$problem, dt$algorithm),
      rep(nrow(dt), nrow(dt)))
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
  # Stack bars
  if (stacked) {
    my.barmode = "stack"
  } else {
    my.barmode = "base"
  }
  p = plotly::layout(
    p,
    yaxis = list(title = "frequency"),
    xaxis = list(title = "ranks"),
    barmode = my.barmode
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
  # Get length of lists
  times = sapply(dt[[list.measure]], function(x) {
    return(length(x))
  })
  # Create new plotly compatible data table
  new.df = data.frame(
    measure = unlist(dt[[list.measure]])
  )
  if ("replication" %in% getMainColumns(dt)) {
    new.df$entry = rep(interaction(dt$problem, dt$algorithm, dt$replication), times)
  } else {
    new.df$entry = rep(interaction(dt$problem, dt$algorithm), times)
  }
  # Create plot
  p = ggplot2::ggplot(data = new.df, ggplot2::aes(measure, fill = entry)) + ggplot2::theme_bw() + ggplot2::labs(fill = "benchmark entry")
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
  # Create new plotly compatible data table
  new.df = data.frame(
    measure = unlist(rank.measure)
  )
  if ("replication" %in% getMainColumns(dt)) {
    new.df$entry = rep(interaction(dt$problem, dt$algorithm, dt$replication), rep(min.iterations, nrow(dt)))
  } else {
    new.df$entry = rep(interaction(dt$problem, dt$algorithm), rep(min.iterations, nrow(dt)))
  }
  # Create plot
  p = ggplot2::ggplot(data = new.df, ggplot2::aes(measure, fill = entry))  + ggplot2::theme_bw() + ggplot2::labs(fill = "benchmark entry")
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

#' @title Create a list scatter plot
#'
#' @description
#' Create a plotly scatter plot out of a benchmarkVis compatible data table.
#' The created scatter plot allows to compare the performances based on the giving performance measure.
#' x-Axis: the benchmark entries.
#' y-Axis: measure.
#'
#' @param dt campatible data table
#' @param list.measure the column name containing the list of a specific measure
#' @param color.by defines the color of the markers. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @return a plotly scatter plot
#' @export
#' @examples
#' createListScatterPlot(microbenchmark.example, "list.values")
createListScatterPlot = function(dt, list.measure, color.by = "algorithm") {
  checkmate::assert_data_table(dt)
  checkmate::assert_string(list.measure)
  checkmate::assert_string(color.by)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  checkmate::assert_true(list.measure %in% getLists(dt))
  # Get length of lists
  times = sapply(dt[[list.measure]], function(x) {
    return(length(x))
  })
  # Create new plotly compatible data table
  new.df = data.frame(
    measure = unlist(dt[[list.measure]]),
    color = rep(dt[[color.by]], times)
  )
  if ("replication" %in% getMainColumns(dt)) {
    new.df$entry = rep(interaction(dt$problem, dt$algorithm, dt$replication), times)
    new.df$text = paste("problem: ", rep(dt$problem, times), "<br>algorithm: ", rep(dt$algorithm, times), "<br>replication: ", rep(dt$replication, times), sep = "")
  } else {
    new.df$entry = rep(interaction(dt$problem, dt$algorithm), times)
    new.df$text = paste("problem: ", rep(dt$problem, times), "<br>algorithm: ", rep(dt$algorithm, times), sep = "")
  }
  # Create plot
  p = plotly::plot_ly(new.df, x = ~ entry, y = ~ measure, type = "scatter", mode = "markers", color = ~ color, marker = list(size = 10), text = ~ text)
  p = plotly::layout(p, xaxis = list(title = "benchmark entry"), yaxis = list(title = list.measure), margin = list(b = 150))
  return(p)
}

#' @title Create a list box plot
#'
#' @description
#' Create a plotly box plot out of a benchmarkVis compatible data table.
#' The created box plot allows for comparison of the benchmark entries based on a given performance measure
#' x-Axis: the benchmark entries.
#' y-Axis: measure.
#'
#' @param dt campatible data table
#' @param list.measure the column name containing the list of a specific measure
#' @param violin if set to TRUE a violin plot instead of boxplot is produced (default: FALSE)
#' @param color.by defines the color of the boxes. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @return a plotly box plot
#' @export
#' @examples
#' createListBoxPlot(microbenchmark.example, "list.values")
createListBoxPlot = function(dt, list.measure, violin = FALSE, color.by = "algorithm") {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(list.measure)
  checkmate::assert_logical(violin)
  checkmate::assert_true(list.measure %in% getLists(dt))
  checkmate::assert_string(color.by)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  # Get length of lists
  times = sapply(dt[[list.measure]], function(x) {
    return(length(x))
  })
  # Create new plotly compatible data table
  new.df = data.frame(
    measure = unlist(dt[[list.measure]]),
    color = rep(dt[[color.by]], times)
  )
  if ("replication" %in% getMainColumns(dt)) {
    new.df$entry = rep(interaction(dt$problem, dt$algorithm, dt$replication), times)
  } else {
    new.df$entry = rep(interaction(dt$problem, dt$algorithm), times)
  }
  # Options
  if (violin) {
    geometry = ggplot2::geom_violin()
  } else {
    geometry = ggplot2::geom_boxplot()
  }
  # Create Plot
  p = ggplot2::ggplot(new.df, ggplot2::aes(x = entry, y = measure, fill = color, colour = color)) +
    geometry + ggplot2::labs(fill = "benchmark entry", col = "benchmark entry") + ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = -45, hjust = 0))
  p = plotly::ggplotly(p)
  p = plotly::layout(
    p,
    xaxis = list(title = "benchmark entry"),
    yaxis = list(title = list.measure),
    margin = list(b = 150)
  )
  return(p)
}

#' @title Create a list measure matrix plot
#'
#' @description
#' Create a list measure matrix plot out of a benchmarkVis compatible data table.
#' The created matrix plot shows all measure results for each benchmark entry.
#' BEWARE! All list measures need to have the same length.
#'
#' @param dt compatible data table
#' @return a measure matrix plot
#' @export
#' @examples
#' createListMeasureMatrixPlot(mlr.benchmark.example)
createListMeasureMatrixPlot = function(dt) {
  # Checks
  checkmate::assert_data_table(dt)
  # Get length of lists
  times = sapply(dt[[getLists(dt)[[1]]]], function(x) {
    return(length(x))
  })
  # Create new plotly compatible data table
  if ("replication" %in% getMainColumns(dt)) {
    entry = rep(interaction(dt$problem, dt$algorithm, dt$replication), times)
  } else {
    entry = rep(interaction(dt$problem, dt$algorithm), times)
  }
  new.df = data.frame(
    entry = entry
  )
  for (list in getLists(dt)) {
    new.df[[list]] = unlist(dt[[list]])
  }
  # Create plot
  p = GGally::ggpairs(new.df, columns = getLists(dt), ggplot2::aes(colour = entry)) + ggplot2::theme_bw()
  p = plotly::ggplotly(p)
  return(p)
}
