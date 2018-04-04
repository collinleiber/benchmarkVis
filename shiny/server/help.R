shiny.help = list(
    "data.format" = c("Appropriate data format", 
                "The app expects your data to be a csv, json or rds file containing a benchmarkVis compatible datatable. Alternatively you can use one of the wrappers provided out-of-the-box: microbenchmark, mlr benchmark, mlr tuning or rbenchmark packages are currently supported"),
    "data.table" = c("Data table", 
                "Aggregation: group values to summarize them. Transformation: apply an element-wise function on a numeric vector (e.g. measure) or a matrix (list of vectors). Aggregation/Transformation functions can be valid basic functions of R (like mean or rank) or your own functions (just type the code of your function in the corr. field). Transformation may be applied to several columns simultaneously. Reset button returns the original state of your data table."),
    "create.plots" = c("Create plots", 
                "Choose if you would like to use original or aggregated data from the previous tab, then choose a plot and its parameters. If you want to save the plot for the future use in the app, just click on the button on the right (you can also edit the name of the plot)"),
    "saved.plots" = c("View saved plots", 
                "Choose one of the plots you have saved on the previous tab. Here you can also delete the plot if you want to (just click on the button)"),
    "compare.plots" = c("Compare plots", 
                "Choose two of the plots you have saved which you want to compare")
)

get.help = function(question, placement='bottom') {
      popify(img(src = 'help.svg', width = "30px", height="30px"), 
        title = shiny.help[question][[1]][1],
        content = shiny.help[question][[1]][2], 
        placement = placement
      )   
}

output$help.import = renderUI({
    get.help('data.format')
})

output$help.table = renderUI({
    get.help('data.table')
})

output$help.plots = renderUI({
    get.help('create.plots')
})

output$help.saved.plots = renderUI({
    get.help('saved.plots')
})

output$help.compare.plots = renderUI({
    get.help('compare.plots')
})

 