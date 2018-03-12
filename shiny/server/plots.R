current.data = reactiveValues(data = NULL)
current.plot = reactiveValues(func = NULL, parameter = list())
observeEvent(input$current.data, {
    if(input$current.data == 'submitted data') {        
        current.data$data = isolate(table$data)
    }
    else {
        current.data$data = isolate(mvalues$matrix)
    }
    current.plot$func = NULL
    current.plot$parameter = NULL
    #DEBUG()
})

observeEvent(input$plotchoice, {
    #DEBUG()
    plot.func = isolate(input$plotchoice)
    if (plot.func!=""){
        current.plot$func = plot.func
        current.plot$parameter = as.list(args(plot.func)) 
    }    
})

observeEvent(input$createtab, {
    print("TODO: new tab")
})

output$plotselection = renderUI({
    selectInput(
        'plotchoice',
        'Choose a plot',
        choices = getValidPlots(current.data$data),
        selected = FALSE,
        multiple = FALSE
    )
})

output$newtab = renderUI({
    h4("Here you can save the plot to an extra tab if you'd like to:")
    textInput(
        'newtab',
        'Select the name for the new tab with this plot',
        current.plot$func
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
    do.call(plot.function, plot.param)
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
    input.react.name = paste0("input$",ui.elem.id)
    if (grepl("measure", param.name) && !grepl("measures", param.name)) {
        ui.elem = selectInput(
            ui.elem.id,
            ui.elem.text,
            choices = getMeasures(isolate(current.data$data)),
            selected = FALSE,
            multiple = FALSE
        )
        observeEvent(eval(parse(text=input.react.name)), {
            current.plot$parameter[[param.name]] = eval(parse(text=input.react.name))
        })
    }
    else if (grepl("measures", param.name)) {
        ui.elem = selectInput(
            ui.elem.id,
            ui.elem.text,
            choices = getMeasures(isolate(current.data$data)),
            selected = FALSE,
            multiple = TRUE
        )
        observeEvent(eval(parse(text=input.react.name)), {
            current.plot$parameter[[param.name]] = eval(parse(text=input.react.name))
        })
    }
    else if (typeof(param[[1]]) == "logical") {
        ui.elem = selectInput(
            ui.elem.id,
            ui.elem.text,
            choices = c('TRUE', 'FALSE'),
            selected = as.character(param[[1]]),
            multiple = FALSE
        )
        observeEvent(eval(parse(text=input.react.name)), {
            current.plot$parameter[[param.name]] = as.logical(eval(parse(text=input.react.name)))
        })
    }
    else {
        ui.elem = textInput(
            ui.elem.id,
            ui.elem.text,
            as.character(param[[1]])
        )
        if(is.numeric(param[[1]])) {
            observeEvent(eval(parse(text=input.react.name)), {
                current.plot$parameter[[param.name]] = as.numeric(eval(parse(text=input.react.name)))
            })
        }        
    }        
      
    return(ui.elem)
}

get.param.reactivevalues = function() {
    all.inputs = names(reactiveValuesToList(input))
    ui.elem.param = sapply(all.inputs, function(x) {startsWith(x, "param_")})
    return(all.inputs[ui.elem.param])
}

DEBUG = function() {
    print("============================================")
    print("input$current.data")
    print(input$current.data)  

    print("input$plotchoice")
    print(input$plotchoice)    
        
    #print("input$parametersSet")
    #print(parametersSet)
    
    print("input$createplot")
    print(input$createplot)
    
    print("input$current.plot")
    print(current.plot$func)
    print(current.plot$parameter)
    print(names(reactiveValuesToList(input)))
                
}