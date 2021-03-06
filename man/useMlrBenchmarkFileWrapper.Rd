% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlrBenchmarkWrapper.R
\name{useMlrBenchmarkFileWrapper}
\alias{useMlrBenchmarkFileWrapper}
\title{Insert mlr benchmark RDS file into benchmarkVis application}
\usage{
useMlrBenchmarkFileWrapper(input.file)
}
\arguments{
\item{input.file}{Path to the input mlr benchmark RDS file}
}
\value{
a data table with the benchmarkVis specific structure
}
\description{
Load the specified file and pass it on the the useMlrBenchmarkWrapper function.
Create a data table useable with the benchmarkVis application out of an mlr benchmark object.
All important imformation will be exluded from the input object and transformed into a appropriate data table
}
