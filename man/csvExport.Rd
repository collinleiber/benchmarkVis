% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/csvJsonHandler.R
\name{csvExport}
\alias{csvExport}
\title{Export a benchmarkVis data table as csv file}
\usage{
csvExport(dt, file.path)
}
\arguments{
\item{dt}{the benchmarkVis data table}

\item{file.path}{path to save the file to}
}
\description{
Export the specified benchmarkVis compatible data table as an csv file. Lists will be converted into a String:
"list(valA = 123, valB = 'xyz')".
Vectors will be converted into a String:
"c(1, 2, 3, 4)"
}
\examples{
csvExport(mlr.benchmark.example, "test.csv")
}
