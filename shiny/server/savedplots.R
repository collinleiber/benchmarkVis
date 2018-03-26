saved.plots.list = list()
current.saved.length = 0

observeEvent(input$createtab, {
    name = isolate(input$newtabname)
    saved.plots.list[[name]] = current.plot$plot
})

output$newPlotSaved = reactive({
    if (saved.plots.list != current.saved.length) {
        current.saved.length = length(saved.plots.list)
        return(TRUE)
    }
    else{
        return(FALSE)
    }
})

outputOptions(output, 'newPlotSaved', suspendWhenHidden = FALSE)