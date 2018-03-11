current.data = reactiveValues(data = NULL)
valid.plots = reactiveValues(plots = NULL)
current.plot = reactiveValues(func = NULL, parameter = list())
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

observeEvent(input$createplot, {
    all.inputs = names(reactiveValuesToList(input))
    ui.elem.param = sapply(all.inputs, function(x) {startsWith(x, "param_")})
    ui.elem.ids = all.inputs[ui.elem.param]
    for (i in 1:length(ui.elem.ids)) {
        arg.name = gsub("param_", "", ui.elem.ids[i])
        observeEvent(eval(parse(text=paste0("input$",ui.elem.ids[i]))), {
            current.plot$parameter[[arg.name]] = eval(parse(text=paste0("input$",ui.elem.ids[i])))
        })
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

output$plot.parameter.selection = renderUI({
    parameter = as.list(args(input$plots))
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
    current.plot$parameter$dt = current.data$data
    do.call(eval(parse(text=input$plots)), current.plot$parameter[!unlist(lapply(current.plot$parameter, is.null))])
})

getValidParameterUI = function(param) {
    if (is.null(param) || is.null(names(param)) || is.null(param[[1]])) {
        return(NULL)
    }
    
    param.name = names(param)
    
    if (param.name == "dt") {
        return(NULL)
    }

    ui.elem.id = paste0('param_', param.name)
    ui.elem.text = paste0('Choose ', param.name)
    if (grepl("measure", param.name) && !grepl("measures", param.name)) {
        ui.elem = selectInput(
            ui.elem.id,
            ui.elem.text,
            choices = getMeasures(current.data$data),
            selected = FALSE,
            multiple = FALSE
        )
    }
    else if (grepl("measures", param.name)) {
        ui.elem = selectInput(
            ui.elem.id,
            ui.elem.text,
            choices = getMeasures(current.data$data),
            selected = FALSE,
            multiple = TRUE
        )
    }
    else if (typeof(param[[1]]) == "logical") {
        ui.elem = selectInput(
            ui.elem.id,
            ui.elem.text,
            choices = c('TRUE', 'FALSE'),
            selected = FALSE,
            multiple = FALSE
        )
    }
    else {
        ui.elem = textInput(
            ui.elem.id,
            ui.elem.text,
            ''
        )
    }           
    return(ui.elem)
}
