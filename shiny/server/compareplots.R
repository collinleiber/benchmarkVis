plots.to.rerender = reactiveValues(plot1 = NULL, plot2 = NULL) 

observeEvent(input$savedPlot1, {
    name = isolate(input$savedPlot1)
    plots.to.rerender$plot1 = saved.plots$plotlist[[name]]
})

observeEvent(input$savedPlot2, {
    name = isolate(input$savedPlot2)
    plots.to.rerender$plot2 = saved.plots$plotlist[[name]]
})

output$selectPlot1 = renderUI({
    selectInput(
        'savedPlot1',
        'Choose one of your saved plots',
        choices = names(saved.plots$plotlist),
        selected = FALSE,
        multiple = FALSE
    )
})

output$selectPlot2 = renderUI({
    selectInput(
        'savedPlot2',
        'Choose one of your saved plots',
        choices = names(saved.plots$plotlist),
        selected = FALSE,
        multiple = FALSE
    )
})

output$rerenderPlot1 = renderUI({
    plot = plots.to.rerender$plot1
    plot.type = class(plot)[1]
    uiplot = switch(plot.type,
        'plotly' = renderPlotly({plot}),
        'chartJSRadar' = radarchart::renderChartJSRadar({plot}),
        renderText({"unknown plot format"})
    )    
    do.call(tagList, c(uiplot))
})

output$rerenderPlot2 = renderUI({
    plot = plots.to.rerender$plot2
    plot.type = class(plot)[1]
    uiplot = switch(plot.type,
        'plotly' = renderPlotly({plot}),
        'chartJSRadar' = radarchart::renderChartJSRadar({plot}),
        renderText({"unknown plot format"})
    )    
    do.call(tagList, c(uiplot))
})

