shiny.help = list(
    "data.format" = c("Appropriate data format", 
                "The app expects your data to be a csv, json or rds file containing a benchmarkVis compatible datatable. Alternatively you can use one of the wrappers provided out-of-the-box: microbenchmark, mlr benchmark, mlr tuning or rbenchmark packages are currently supported"),
    "aggregation" = c("Aggregation", "You can use any aggregatd function contained in base R or define your own function which should return one value for a list. The input expected is a function just as if you would define any function in R")
)

get.help = function(question, placement='bottom') {
   div(style="width: 50 px; height: 30px;",
      popify(imageOutput("help", width = "30px", height="30px"), 
        title = shiny.help[question][[1]][1],
        content = shiny.help[question][[1]][2], 
        placement = placement
      )
   )
}

output$help.import = renderUI({
    get.help('data.format', 'bottom')
})


 