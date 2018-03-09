#' @title Create a bar plot
#'
#' @description
#' Create a plotly bar plot out of a benchmarkVis compatible data table.
#' The created bar plot shows the measure result for each benchmark entry.
#' x-Axis: the measure.
#' y-Axis: the benchmark entries.
#'
#' @param dt compatible data table
#' @param measure the column name containing the results of a measure
#' @param group.problems if true group results by problem (default: FALSE)
#' @return a plotly bar plot
#' @export
#' @examples
#' createBarPlot(mlr.benchmark.example, "measure.mmce.test.mean", FALSE)
createBarPlot = function(dt, measure, group.problems = FALSE) {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_string(measure)
  checkmate::assert_logical(group.problems)
  checkmate::assert_true(measure %in% getMeasures(dt))
  # Create new data frame
  new.df = data.frame(
    measure = dt[[measure]],
    problem = dt$problem,
    algorithm = dt$algorithm
  )
  # Create plot
  if (group.problems) {
    p = plotly::plot_ly(
      new.df,
      x = ~ measure,
      y = ~ problem,
      type = "bar",
      orientation = "h",
      color = ~ algorithm
    )
  } else {
    p = plotly::plot_ly(
      new.df,
      x = ~ measure,
      y = ~ reorder(interaction(problem, algorithm), measure),
      type = "bar",
      orientation = "h",
      color = ~ algorithm
    )
  }
  p = plotly::layout(
    p,
    yaxis = list(title = ""),
    xaxis = list(title = measure),
    margin = list(l = 180)
  )
  return(p)
}

#' @title Create a rank matrix bar plot
#'
#' @description
#' Create a plotly rank matrix bar plot out of a benchmarkVis compatible data table.
#' The created bar plot shows the frequency of the ranks each algorithm achieves depending on problem and measure.
#' x-Axis: the ranks.
#' y-Axis: the frequency.
#'
#' @param dt compatible data table
#' @param ignore.measures a vector of strings describing which measures to leave out of the plot (default: empty)
#' @return a plotly rank matrix bar plot
#' @export
#' @examples
#' createRankMatrixBarPlot(mlr.benchmark.example, c("measure.timetrain.test.mean","measure.mmce.test.mean"))
createRankMatrixBarPlot = function(dt, ignore.measures = vector()) {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_character(ignore.measures)
  # Create new data frame with all ranks
  new.df = data.frame(algorithm = dt$algorithm)
  # Get ranks for each measure and each problem
  for (measure in getMeasures(dt)) {
    if (!measure %in% ignore.measures) {
      tmp = vector()
      for (p in levels(dt$problem)) {
        tmp = c(tmp, rank(dt[dt$problem == p, ][[measure]], ties.method = "min"))
      }
      new.df[[measure]] = tmp
    }
  }
  # Create final data frame
  algo.count = length(levels(new.df$algorithm))
  compact.df = data.frame(algorithm = rep(levels(new.df$algorithm), rep(algo.count, algo.count)),
    rank = as.factor(rep(seq(algo.count), algo.count)))
  # Count ranks
  counter = vector()
  for (algo in levels(new.df$algorithm)) {
    for (i in seq(algo.count)) {
      count = 0
      for (measure in getMeasures(new.df)) {
        count = count + nrow(new.df[new.df[[measure]] == i &
            new.df$algorithm == algo, ])
      }
      counter = c(counter, count)
    }
  }
  compact.df$count = counter
  # Create plot
  p = plotly::plot_ly(
    compact.df,
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
