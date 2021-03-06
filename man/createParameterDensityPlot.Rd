% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parameterPlots.R
\name{createParameterDensityPlot}
\alias{createParameterDensityPlot}
\title{Create a parameter density plot}
\usage{
createParameterDensityPlot(dt, measure, parameter.column, parameter,
  stacked = FALSE)
}
\arguments{
\item{dt}{campatible data table}

\item{measure}{the input measure on the x axis}

\item{parameter.column}{the parameter column which contains the parameter}

\item{parameter}{the parameter you want to examine}

\item{stacked}{defines if the density curves should be stacked. Alternative is transparent (default: FALSE)}
}
\value{
a plotly density plot
}
\description{
This plot is just usable if at least one parameter column is contained in the data table.
Create a plotly density plot out of a benchmarkVis compatible data table.
The created density chart shows the distribution of the input measure based on the specified parameter..
x-Axis: the measure
y-Axis: the density.
}
\examples{
createParameterDensityPlot(mlr.benchmark.example, "measure.mmce.test.mean", "problem.parameter", "size")
}
