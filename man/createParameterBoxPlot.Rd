% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameterPlots.R
\name{createParameterBoxPlot}
\alias{createParameterBoxPlot}
\title{Create a parameter box plot}
\usage{
createParameterBoxPlot(dt, measure, parameter.column, parameter,
  violin = FALSE)
}
\arguments{
\item{dt}{campatible data table}

\item{measure}{the measure on the y axis}

\item{parameter.column}{the parameter column which contains the parameter}

\item{parameter}{the parameter you want to examine}

\item{violin}{if set to TRUE a violin plot instead of boxplot is produced (default: FALSE)}
}
\value{
a plotly box plot
}
\description{
This plot is just usable if at least one parameter column is contained in the data table.
Create a plotly box plot out of a benchmarkVis compatible data table.
The created box chart shows the distribution for the input measure based on the specified parameter.
x-Axis: different values of the parameter.
y-Axis: the measure.
}
\examples{
createParameterBoxPlot(mlr.benchmark.example, "measure.mmce.test.mean", "problem.parameter", "size")
}
