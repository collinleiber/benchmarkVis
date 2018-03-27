saved.plots = reactiveValues(plotlist = list()) 
plot.to.rerender = reactiveValues(plot = NULL) 

output$plotsSaved = reactive({
    return(length(saved.plots$plotlist) > 0)
 })

outputOptions(output, 'plotsSaved', suspendWhenHidden = FALSE)

observeEvent(input$createtab, {
    name = isolate(input$newtabname)
    saved.plots$plotlist[[name]] = isolate(current.plot$plot)
})

observeEvent(input$savedPlot, {
    name = isolate(input$savedPlot)
    plot.to.rerender$plot = saved.plots$plotlist[[name]]
})

observeEvent(input$deletePlot, {
    name = isolate(input$savedPlot)
    saved.plots$plotlist[[name]] = NULL
    plot.to.rerender$plot = NULL 
})

output$savedPlotSelection = renderUI({
    selectInput(
        'savedPlot',
        'Choose one of your saved plots',
        choices = names(saved.plots$plotlist),
        selected = FALSE,
        multiple = FALSE
    )
})

output$rerenderPlot = renderUI({
    plot.type = class(plot.to.rerender$plot)[1]
    uiplot = switch(plot.type,
        'plotly' = renderPlotly({plot.to.rerender$plot}),
        'chartJSRadar' = radarchart::chartJSRadarOutput(radarchart::renderChartJSRadar({plot.to.rerender$plot}), width = "450", height = "300"),
        renderText({"unknown plot format"})
    )    
    do.call(tagList, c(uiplot))
})

