current.data = reactiveValues(data = NULL)
current.plot = reactiveValues(func = NULL, parameter = list(), plot = NULL)

observeEvent(input$current.data, {
    if(input$current.data == 'submitted data') {        
        current.data$data = isolate(table$data)
    }
    else {
        current.data$data = isolate(mvalues$matrix)
    }
    current.plot$func = NULL
    current.plot$parameter = NULL
})

observeEvent(input$plotchoice, {
    plot.func = unprettifyPlotName(isolate(input$plotchoice))
    if (plot.func!=""){
        current.plot$func = plot.func
        current.plot$parameter = as.list(args(plot.func)) 
    }    
})

output$plotselection = renderUI({
    valid = getValidPlots(current.data$data)
    selectInput(
        'plotchoice',
        'Choose a plot',
        choices = unname(getPrettyPlotList(valid)),
        selected = FALSE,
        multiple = FALSE
    )
})

output$plot.parameter.selection = renderUI({
    parameter = as.list(args(current.plot$func))
    uilist = list()
    for (i in 1:length(parameter)) {
        ui.elem = getValidParameterUI(parameter[i])        
        if (!is.null(ui.elem)) {
            uilist = c(uilist,ui.elem)            
        }        
    } 
    do.call(tagList, uilist)  
})

output$plot = renderPlotly({
    plot.function = eval(parse(text=current.plot$func))
    current.plot$parameter$dt = isolate(current.data$data)
    plot.param = isolate(current.plot$parameter[!unlist(lapply(current.plot$parameter, is.null))])
    current.plot$plot = do.call(plot.function, plot.param)
    current.plot$plot
})

output$radar = radarchart::renderChartJSRadar({
    current.plot$plot = createRadarPlot(isolate(current.data$data))
    current.plot$plot
})

output$newtab = renderUI({
    h4("Here you can save the plot to an extra tab if you'd like to:")
    textInput(
        'newtabname',
        'Select the name for the new tab with this plot',
        isolate(input$plotchoice)
    )
})