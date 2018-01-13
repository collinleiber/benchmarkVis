# Get the correct count of measures within the dataframe
# Calculation is (#rows - 6) / 2
# Reason: 6 non measure columns, half of measure columns is for replication
getMeasureCount = function(df) {
  return((ncol(df) - 6) / 2)
}
