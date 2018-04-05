# benchmarkVis - Benchmark Visualizations in R

[![Build Status](https://travis-ci.org/collinleiber/benchmarkVis.svg?branch=master)](https://travis-ci.org/collinleiber/benchmarkVis) [![codecov](https://codecov.io/github/collinleiber/benchmarkVis/branch/master/graphs/badge.svg)](https://codecov.io/github/collinleiber/benchmarkVis) 

benchmarkVis is a R package to visualize benchmark results in different ways. It is working with standard csv, json and files and can also be combined with several benchmark R packages like [microbenchmark](https://github.com/joshuaulrich/microbenchmark/) , [rbenchmark](https://github.com/eddelbuettel/rbenchmark) or [mlr](https://github.com/mlr-org/mlr) throuhgh integrated wrappers. Thanks to the universal input table structure it is also possible to integrate results from  [batchtools](https://github.com/mllg/batchtools) or frameworks outside the R language like pythons [scikit-learn](http://scikit-learn.org).

# Getting Started

* Install the development version
    ```r
    devtools::install_github("collinleiber/benchmarkVis")
    ```
* Take a look into the [Wiki](https://github.com/collinleiber/benchmarkVis/wiki) for a full tutorial
	
# Description

Benchmarking is a good way to compare the performances of different algorithms. To evaluate the results often the same procedures are used. But even though different benchmarks normally contain similar information, the structure can differ significantly. This increases the effort to visualize and analyze them. You have to do the same creation steps over and over again with just a few little changes. At this point the benchmarkVis package comes into play. It aims to convert various formats into a default data table which then can be visualized in multiple ways. 

# Compatible data table

| problem | problem.parameter | algorithm | algorithm.parameter | replication | replication.parameter | measure.1 | measure.2 | list.1 | list.2 |
|---|---|---|---|---|---|---|---|---|---|
| factor | list | factor | list | factor | list | numeric | numeric | vector | vector |
| mandatory | optional | mandatory | optional | optional | optional | optional | optional | optional | optional |

As you can see, each column has a fixed name and data type. Also some of the columns are optional while others are mandatory.
The table can contain any number of measures and lists. It is important that at least one column of type measure or list is contained and that the column names start with "measure." / "list.".

One special case occurs if you change algorithm parameters through multiple iterations. If this is the case you need to add the numeric field `iteration` to the algorithm.parameters list of the corresponding algorithm. It is important that this value is defined for every entry of the algorithm and that no value occurs multiple times.

# Quick Start

In this example we will use one of the provided wrappers (in this case the wrapper for microbenchmarks) as input data and create a bar plot and a list line chart.

Create input data:

``` r
library(benchmarkVis)
library(microbenchmark)
x = runif(100)
benchmark = microbenchmark(sqrt(x), x ^ 0.5)

table = useMicrobenchmarkWrapper(benchmark)
```

See a list with all visualizations usable with the input data:

``` r
getValidPlots(table)
```

Create Plots:

``` r
createBarPlot(table, "measure.mean")
createListLinePlot(table, "list.values", "mean", TRUE)
```

### Next steps

For more complex examples take a look at the [Example Use Cases](https://github.com/collinleiber/benchmarkVis/wiki/Tutorial:-5.-Example-Use-Cases).


If you want to use your own data you can import csv, json and rds files:

CSV:

``` r
table = csvImport("PATH.TO.CSV.FILE")
```

JSON:

``` r
table = jsonImport("PATH.TO.JSON.FILE")
```

RDS:

```r
table = rdsImport("PATH.TO.RDS.FILE")
```

Check out our tutorial in the [Wiki](https://github.com/collinleiber/benchmarkVis/wiki) for detailed information.
