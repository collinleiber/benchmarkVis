# benchmarkVis - Benchmark Visualizations in R

[![Build Status](https://travis-ci.org/collinleiber/benchmarkVis.svg?branch=master)](https://travis-ci.org/collinleiber/benchmarkVis)

benchmarkVis is a R package to visualize benchmark results in different ways. It is working with standard csv files and can also be combined with several benchmark R packages like [microbenchmark](https://github.com/joshuaulrich/microbenchmark/) , [rbenchmark](https://github.com/eddelbuettel/rbenchmark) or [mlr](https://github.com/mlr-org/mlr). 

# Getting Started

* Install the development version
    ```splus
    devtools::install_github("collinleiber/benchmarkVis")
    ```
* Take a look into the [Wiki](https://github.com/collinleiber/benchmarkVis/wiki) for more information
* Continous Integration by [Travis](https://travis-ci.org/collinleiber/benchmarkVis)
	
# Description

Benchmarking is a good way to compare the performances of different algorithms. To evaluate the results often the same procedures are used. But even though different benchmarks normally contain similar information, the structure can differ significantly. This increases the effort to visualize and analyze them. You have to do the same creation steps over and over again with just a few little changes. At this point the benchmarkVis package comes into play. It aims to convert various formats into a default data table which then can be visualized in multiple ways. 

# Compatible data table

| problem | problem.parameter | algorithm | algorithm.parameter | replication | replication.parameter | measure.1 | measure.2 | list.1 | list.2 |
|---|---|---|---|---|---|---|---|---|---|
| factor | list | factor | list | factor | list | numeric | numeric | vector | vector |
| mandatory | mandatory | mandatory | mandatory | mandatory | mandatory | optional | optional | optional | optional |

As you can see, each column has a fixed name and data type. Also some of the columns are optional while others are mandatory.
The table can contain any number of measures and lists. It is just important that at least one column of type measure or list is contained and that the column names start with "measure." / "list.".

One special case occurs if you change algorithm parameters through multiple iterations. If this is the case you need to add the numeric field `iteration` to the algorithm.parameters list of the specified algorithm. It is important that this value is defined for every entry of the algorithm and that no value occurs multiple times.
