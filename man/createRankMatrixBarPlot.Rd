% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/barPlot.R
\name{createRankMatrixBarPlot}
\alias{createRankMatrixBarPlot}
\title{Create a rank matrix bar plot}
\usage{
createRankMatrixBarPlot(dt, ignore.measures = vector(), stacked = TRUE,
  color.by = "algorithm", group.by = "problem")
}
\arguments{
\item{dt}{compatible data table}

\item{ignore.measures}{a vector of strings describing which measures to leave out of the plot (default: empty)}

\item{stacked}{defines if the bars should be stacked (default: TRUE)}

\item{color.by}{the column to color the bars with. Possibilities: "algorithm", "problem", "replication" (default: "algorithm")}

\item{group.by}{the column to group the bars by. Possibilities: "algorithm", "problem", "replication" (default: "problem")}
}
\value{
a plotly rank matrix bar plot
}
\description{
Create a plotly rank matrix bar plot out of a benchmarkVis compatible data table.
The created bar plot shows the frequency of the ranks each input achieves depending on a group and measures.
The value defined in "color.by" can get one rank per value in "group.by" and measure.
x-Axis: the ranks.
y-Axis: the frequency.
}
\examples{
createRankMatrixBarPlot(mlr.benchmark.example, c("measure.timetrain.test.mean","measure.mmce.test.mean"))
}
