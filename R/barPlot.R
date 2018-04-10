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
#' @param color.by the column to color the bars with. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @param group.color instead of coloring the column specified by color.by it will be grouped. Color will be chosen from the remaining main columns instead (default: FALSE)
#' @param stacked defines if the grouped bars should be stacked. Just working with "group.color" = TRUE (default: FALSE)
#' @return a plotly bar plot
#' @export
#' @examples
#' createBarPlot(mlr.benchmark.example, "measure.mmce.test.mean")
createBarPlot = function(dt, measure, color.by = "algorithm", group.color = FALSE, stacked = FALSE) {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_logical(stacked)
  checkmate::assert_string(measure)
  checkmate::assert_true(measure %in% getMeasures(dt))
  checkmate::assert_logical(group.color)
  checkmate::assert_string(color.by)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  # Create new data frame
  new.df = data.frame(
    measure = dt[[measure]],
    color = dt[[color.by]]
  )
  #Group the color
  if (group.color) {
    # Pick color by remaining main columns
    if ("replication" %in% getMainColumns(dt)) {
      if (color.by == "algorithm") {
        new.df$inter = interaction(dt$problem, dt$replication)
      } else if (color.by == "problem") {
        new.df$inter = interaction(dt$algorithm, dt$replication)
      } else if (color.by == "replication") {
        new.df$inter = interaction(dt$algorithm, dt$problem)
      }
    } else if (color.by == "problem") {
      new.df$inter = dt$algorithm
    } else if (color.by == "algorithm") {
      new.df$inter = dt$problem
    }
    # Create plot
    p = plotly::plot_ly(
      new.df,
      x = ~ measure,
      y = ~ color,
      type = "bar",
      orientation = "h",
      color = ~ inter
    )
  # Do not group colors
  } else {
    # Interact all main columns
    if ("replication" %in% getMainColumns(dt)) {
      new.df$inter = interaction(dt$problem, dt$algorithm, dt$replication)
    } else {
      inter = interaction(dt$problem, dt$algorithm)
    }
    # Create plot
    p = plotly::plot_ly(
      new.df,
      x = ~ measure,
      y = ~ reorder(inter, measure),
      type = "bar",
      orientation = "h",
      color = ~ color
    )
  }
  # Stack bars
  if (stacked & group.color) {
    my.barmode = "stack"
  } else {
    my.barmode = "base"
  }
  p = plotly::layout(
    p,
    yaxis = list(title = ""),
    xaxis = list(title = measure),
    margin = list(l = 180),
    barmode = my.barmode
  )
  return(p)
}

#' @title Create a rank matrix bar plot
#'
#' @description
#' Create a plotly rank matrix bar plot out of a benchmarkVis compatible data table.
#' The created bar plot shows the frequency of the ranks each input achieves depending on a group and measures.
#' The value defined in "color.by" can get one rank per value in "group.by" and measure.
#' x-Axis: the ranks.
#' y-Axis: the frequency.
#'
#' @param dt compatible data table
#' @param ignore.measures a vector of strings describing which measures to leave out of the plot (default: empty)
#' @param stacked defines if the bars should be stacked (default: TRUE)
#' @param color.by the column to color the bars with. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")
#' @param group.by the column to group the bars by. Possibilities: "algorithm", "problem", "replication" (default: "problem")
#' @return a plotly rank matrix bar plot
#' @export
#' @examples
#' createRankMatrixBarPlot(mlr.benchmark.example, c("measure.timetrain.test.mean","measure.mmce.test.mean"))
createRankMatrixBarPlot = function(dt, ignore.measures = vector(), stacked = TRUE, color.by = "algorithm", group.by = "problem") {
  # Checks
  checkmate::assert_data_table(dt)
  checkmate::assert_logical(stacked)
  checkmate::assert_character(ignore.measures)
  checkmate::assert_true(all(ignore.measures %in% getMeasures(dt)))
  checkmate::assert_string(color.by)
  checkmate::assert_string(group.by)
  checkmate::assert_true(color.by %in% getMainColumns(dt))
  checkmate::assert_true(group.by %in% getMainColumns(dt))
  # Create new data frame with all ranks
  new.df = data.frame(color = dt[[color.by]])
  # Get ranks for each measure and each group
  for (measure in getMeasures(dt)) {
    if (!measure %in% ignore.measures) {
      tmp = vector()
      for (p in levels(dt[[group.by]])) {
        tmp = c(tmp, rank(dt[dt[[group.by]] == p, ][[measure]], ties.method = "min"))
      }
      new.df[[measure]] = tmp
    }
  }
  # Create final data frame
  color.count = length(levels(new.df$color))
  compact.df = data.frame(color = rep(levels(new.df$color), rep(color.count, color.count)),
    rank = as.factor(rep(seq(color.count), color.count)))
  # Count ranks
  counter = vector()
  for (color in levels(new.df$color)) {
    for (i in seq(color.count)) {
      count = 0
      for (measure in getMeasures(new.df)) {
        count = count + nrow(new.df[new.df[[measure]] == i &
            new.df$color == color, ])
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
    color = ~ color,
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
