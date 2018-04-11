getValidParameterUI = function(param) {
  if (is.null(param) ||
      is.null(names(param)) || is.null(param[[1]])) {
    return(NULL)
  }

  param.name = names(param)

  if (param.name == "dt") {
    return(NULL)
  }

  ui.elem.id = paste0("param_", param.name)
  ui.elem.text = param.name
  #ui.elem.text = paste0("Choose ", param.name)
  input.react.name = paste0("input$", ui.elem.id)
  if (grepl("measures", param.name)) {
    ui.elem = selectInput(
      ui.elem.id,
      ui.elem.text,
      choices = getMeasures(isolate(current.data$data)),
      selected = FALSE,
      multiple = TRUE
    )
    observeEvent(eval(parse(text = input.react.name)), {
      current.plot$parameter[[param.name]] = eval(parse(text = input.react.name))
    })
  }
  else if (grepl("list.measure", param.name)) {
    ui.elem = selectInput(
      ui.elem.id,
      ui.elem.text,
      choices = getLists(isolate(current.data$data)),
      selected = FALSE,
      multiple = FALSE
    )
    observeEvent(eval(parse(text = input.react.name)), {
      current.plot$parameter[[param.name]] = eval(parse(text = input.react.name))
    })
  }
  else if (grepl("measure", param.name)) {
    ui.elem = selectInput(
      ui.elem.id,
      ui.elem.text,
      choices = getMeasures(isolate(current.data$data)),
      selected = FALSE,
      multiple = FALSE
    )
    observeEvent(eval(parse(text = input.react.name)), {
      current.plot$parameter[[param.name]] = eval(parse(text = input.react.name))
    })
  }
  else if (grepl("color.by", param.name) ||
           grepl("group.by", param.name) ||
           grepl("style.by", param.name)) {
    ui.elem = selectInput(
      ui.elem.id,
      ui.elem.text,
      choices = getMainColumns(isolate(current.data$data)),
      selected = FALSE,
      multiple = FALSE
    )
    observeEvent(eval(parse(text = input.react.name)), {
      current.plot$parameter[[param.name]] = eval(parse(text = input.react.name))
    })
  }
  else if (grepl("interaction.column", param.name)) {
    ui.elem = selectInput(
      ui.elem.id,
      ui.elem.text,
      choices = c("none", getMainColumns(isolate(current.data$data))),
      selected = FALSE,
      multiple = FALSE
    )
    observeEvent(eval(parse(text = input.react.name)), {
      current.plot$parameter[[param.name]] = eval(parse(text = input.react.name))
    })
  }
  else if (grepl("cumulative.function", param.name)) {
    ui.elem = selectInput(
      ui.elem.id,
      ui.elem.text,
      choices = c("id", "max", "min", "mean"),
      selected = FALSE,
      multiple = FALSE
    )
    observeEvent(eval(parse(text = input.react.name)), {
      current.plot$parameter[[param.name]] = eval(parse(text = input.react.name))
    })
  }
  else if (grepl("iteration.algorithm", param.name)) {
    ui.elem = selectInput(
      ui.elem.id,
      ui.elem.text,
      choices = getIterationAlgorithms(isolate(current.data$data)),
      selected = FALSE,
      multiple = FALSE
    )
    observeEvent(eval(parse(text = input.react.name)), {
      current.plot$parameter[[param.name]] = eval(parse(text = input.react.name))
    })
  }
  else if (typeof(param[[1]]) == "logical") {
    ui.elem = selectInput(
      ui.elem.id,
      ui.elem.text,
      choices = c("TRUE", "FALSE"),
      selected = as.character(param[[1]]),
      multiple = FALSE
    )
    observeEvent(eval(parse(text = input.react.name)), {
      current.plot$parameter[[param.name]] = as.logical(eval(parse(text = input.react.name)))
    })
  }
  else if (grepl("parameter", param.name)) {
    ui.elem = selectInput(
      ui.elem.id,
      ui.elem.text,
      choices = names(isolate(current.data$data)$algorithm.parameter[[1]]),
      selected = FALSE,
      multiple = FALSE
    )
    observeEvent(eval(parse(text = input.react.name)), {
      current.plot$parameter[[param.name]] = eval(parse(text = input.react.name))
    })
  }
  else {
    ui.elem = textInput(ui.elem.id,
                        ui.elem.text,
                        as.character(param[[1]]))
    if (is.numeric(param[[1]])) {
      observeEvent(eval(parse(text = input.react.name)), {
        current.plot$parameter[[param.name]] = as.numeric(eval(parse(text = input.react.name)))
      })
    }
    else{
      observeEvent(eval(parse(text = input.react.name)), {
        current.plot$parameter[[param.name]] = as.character(eval(parse(text = input.react.name)))
      })
    }
  }

  return(ui.elem)
}

get.param.reactivevalues = function() {
  all.inputs = names(reactiveValuesToList(input))
  ui.elem.param = sapply(all.inputs, function(x) {
    startsWith(x, "param_")
  })
  return(all.inputs[ui.elem.param])
}
