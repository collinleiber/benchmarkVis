current.data = reactiveValues(data = NULL)
valid.plots = reactiveValues(plots = NULL)
observeEvent(input$currentdata, {
    if(input$currentdata == 'submitted data') {        
        current.data$data = table$data
    }
    else {
        current.data$data = aggregated.data$dt
    }
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

output$plot = renderPlotly({
    parameter = c(dt = current.data$data, input$plotparameter)
    do.call(eval(parse(text=input$plots)), parameter)
})