# Get the correct count of measures within the dataframe
# Calculation is: #rows - #relication.measures - 6
# Reason: X replication measure columns and 6 non measure columns
getMeasureCount = function(df) {
  return(ncol(df) - getReplicationMeasureCount(df) - 6)
}

# Get the correct count of replication measures within the dataframe
# Calculation is: #columns.starting.with.replication - 2
# Reason: All columns starting with "repliction" - replication - replication.parameter
getReplicationMeasureCount = function(df) {
  replication.count = subset(names(df), startsWith(names(df), "replication"))
  return(length(replication.count) - 2)
}

# Return a empty list containing multiple lists of type input.list (Default = list()).
# Number of repitition can be set with repitition.count (Default is 1).
repList = function(input.list = list(), repitition.count = 1) {
  mylist = list()
  for (i in 1:repitition.count) {
    # Create copy of list
    mylist[[i]] = lapply(input.list, function(x) {x})
  }
  return(mylist)
}
