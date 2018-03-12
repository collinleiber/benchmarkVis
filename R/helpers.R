# Get the names of measure columns within the data table
# Measures columns start with "measure."
getMeasures = function(dt) {
  return(subset(names(dt), startsWith(names(dt), "measure.")))
}

# Get the number of measure columns within the data table
# Measures columns start with "measure."
getMeasuresCount = function(dt) {
  return(length(getMeasures(dt)))
}

# Get the names of list columns (eg replication measures) within the data table
# List columns start with "list."
getLists = function(dt) {
  return(subset(names(dt), startsWith(names(dt), "list.")))
}

# Get the number of list columns (eg replication measures) within the data table
# List columns start with "list."
getListsCount = function(dt) {
  return(length(getLists(dt)))
}

# Get the names of algorithms with multiple iterations within the data table
# Algorithms for which the parameters contain "iteration" field
getIterationAlgorithms = function(dt) {
  iteration.algorithms = vector()
  for (row in nrow(dt)) {
    if ("iteration" %in% names(dt[[row, "algorithm.parameter"]]) &&
        !as.character(dt[[row, "algorithm"]]) %in% iteration.algorithms) {
      iteration.algorithms = c(iteration.algorithms, as.character(dt[[row, "algorithm"]]))
    }
  }
  return(iteration.algorithms)
}

# Get the number of algorithms with multiple iterations within the data table
# Algorithms for which the parameters contain "iteration" field
getIterationAlgorithmsCount = function(dt) {
  return(length(getIterationAlgorithms(dt)))
}

# Return a list containing multiple lists of type input.list (Default = list()).
# Number of repitition can be set with repitition.count (Default is 1).
repList = function(input.list = list(),
  repitition.count = 1) {
  mylist = list()
  for (i in 1:repitition.count) {
    # Create copy of list (rep would ignore empty list)
    mylist[[i]] = lapply(input.list, function(x) {
      x
    })
  }
  return(mylist)
}

# Return a list with the originallist changed by the input cumulative function.
# Cumulative functions can be id, min, max, mean
getCumulativeValues = function(list, cumulative.function) {
  checkmate::assert_string(cumulative.function)
  checkmate::assert_numeric(list)
  checkmate::assert_true(cumulative.function %in% c("id", "max", "min", "mean"))
  # Return input list for id
  if (cumulative.function == "id") {
    return(list)
  }
  # Save states
  val = NA
  count = 0
  new.list = vector()
  for (i in seq(list)) {
    x = list[i]
    if (cumulative.function == "max") {
      if (is.na(val)) {
        val = -Inf
      }
      val = max(val, x)
    } else if (cumulative.function == "min") {
      if (is.na(val)) {
        val = Inf
      }
      val = min(val, x)
    } else if (cumulative.function == "mean") {
      if (is.na(val)) {
        val = 0
      }
      val = (val * count + x) / (count + 1)
      count = count + 1
    }
    new.list[i] = val
  }
  # Return new created vector
  return(new.list)
}

#Return a list of plots suitable for the given data
#(depends on the presence/absence of measures, iteration and list data)
getValidPlots = function(dt) {
  all.plots = listPlots() #all plots available in the package
  valid.plots = list()
  if (getMeasuresCount(dt) > 0) {
    measures.plots = sapply(all.plots, function(plot) {!grepl("Iteration", plot) && !grepl("List", plot)})
    valid.plots = c(all.plots[measures.plots])
  }
  if (getIterationAlgorithmsCount(dt) > 0) {
    iteration.plots = sapply(all.plots, function(plot) {grepl("Iteration", plot)})
    valid.plots = c(valid.plots, all.plots[iteration.plots])
  }
  if (getListsCount(dt) > 0) {
    list.plots = sapply(all.plots, function(plot) {grepl("List", plot)})
    valid.plots = c(valid.plots, all.plots[list.plots])
  }
  return(valid.plots)
}
