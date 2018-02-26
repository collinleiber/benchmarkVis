# Get the names of measure columns within the data table
# Measures columns start with "measure."
getMeasures = function(dt) {
  return(subset(names(dt), startsWith(names(dt), "measure.")))
}

# Get the names of list columns (eg replication measures) within the data table
# List columns start with "list."
getLists = function(dt) {
  return(subset(names(dt), startsWith(names(dt), "list.")))
}

# Get the number of measure columns within the data table
# Measures columns start with "measure."
getMeasuresCount = function(dt) {
  return(length(getMeasures(dt)))
}

# Get the number of list columns (eg replication measures) within the data table
# List columns start with "list."
getListsCount = function(dt) {
  return(length(getLists(dt)))
}

# Return a list containing multiple lists of type input.list (Default = list()).
# Number of repitition can be set with repitition.count (Default is 1).
repList = function(input.list = list(), repitition.count = 1) {
  mylist = list()
  for (i in 1:repitition.count) {
    # Create copy of list
    mylist[[i]] = lapply(input.list, function(x) {x})
  }
  return(mylist)
}
