current.data = reactiveValues(data = NULL)
valid.plots = reactiveValues(plots = NULL)
current.plot = reactiveValues(func = NULL, parameter = NULL)
observeEvent(input$current.data, {
    if(input$current.data == 'submitted data') {        
        current.data$data = table$data
    }
    else {
        current.data$data = aggregated.data$dt
    }
})
observeEvent(input$plots, {
    current.plot$func = input$plots
    current.plot$parameter = as.list(args(input$plots))
})

output$plotselection = renderUI({
    selectInput(
        'plots',
        'Choose a plot',
        choices = getValidPlots(current.data$data),
        selected = FALSE,
        multiple = FALSE
    )
})

output$plot.parameter.selection = renderUI({
    parameter = as.list(args(input$plots))
    uilist = list()
    for (i in 2:length(parameter)) {
        #if(names(parameter[i]) == "dt")
        #    next
        uilist = c(uilist,getValidUI(parameter[i]))
         observeEvent(input$paste('param_', names(parameter[i])), {
            cat("klappt")
        })
    }    
    do.call(tagList, uilist)   
})

output$plot = renderPlotly({
    parameter = c(dt = current.data$data, input$plotparameter)
    print(input$plots)
    print(parameter)
    print(typeof(parameter$dt))
    do.call(eval(parse(text=input$plots)), list(parameter))
})

getValidUI = function(param) {
    if (is.null(param) || is.null(names(param)) || is.null(param[[1]])) {
        return()
    }
    
    param.name = names(param)
    if (grepl("measure", param.name) && !grepl("measures", param.name)) {
        ui.elem = selectInput(
            paste('param_', param.name),
            paste('Choose ', param.name),
            choices = getMeasures(current.data$data),
            selected = FALSE,
            multiple = FALSE
        )
    }
    else if (grepl("measures", param.name)) {
        ui.elem = selectInput(
            paste('param_', param.name),
            paste('Choose ', param.name),
            choices = getMeasures(current.data$data),
            selected = FALSE,
            multiple = TRUE
        )
    }
    else if (typeof(param[[1]]) == "logical") {
        ui.elem = selectInput(
            paste('param_', param.name),
            paste('Choose ', param.name),
            choices = c('TRUE', 'FALSE'),
            selected = FALSE,
            multiple = FALSE
        )
    }
    else {
        ui.elem = textInput(
            paste('param_', param.name),
            paste('Choose ', param.name),
            ''
        )
    }
           
    return(ui.elem)
}
