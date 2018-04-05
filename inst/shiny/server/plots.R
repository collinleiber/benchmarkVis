current.data = reactiveValues(data = NULL)
current.plot = reactiveValues(func = NULL,
                              parameter = list(),
                              plot = NULL)

observeEvent(input$current.data, {
  if (input$current.data == "submitted data") {
    current.data$data = table$data
  }
  else if (input$current.data == "aggregated data") {
    current.data$data = aggregated.data$dt
  }
  updateSelectInput(session, "plotchoice",
                    selected = "none (choose a plot)")
})

observeEvent(input$plotchoice, {
  if (input$plotchoice != "none (choose a plot)") {
    plot.func = unprettifyPlotName(input$plotchoice)
    if (plot.func != "") {
      current.plot$func = plot.func
      current.plot$parameter = as.list(args(plot.func))
    }
  }
  else {
    current.plot$func = NULL
    current.plot$parameter = list()
  }
})

observeEvent(input$createplot, {
  plot.function = eval(parse(text = current.plot$func))
  current.plot$parameter$dt = current.data$data
  plot.param = current.plot$parameter[!unlist(lapply(current.plot$parameter, is.null))]
  current.plot$plot = do.call(plot.function, plot.param)
})

output$plotSelected = reactive({
  return(!is.null(current.plot$func))
})

outputOptions(output, "plotSelected", suspendWhenHidden = FALSE)

output$plotselection = renderUI({
  valid = getValidPlots(current.data$data)
  selectInput(
    "plotchoice",
    "Choose a plot",
    choices = c("none (choose a plot)", unname(getPrettyPlotList(valid))),
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
      uilist = c(uilist, ui.elem)
    }
  }
  uilist = lapply(uilist, function(item) {
    item = item[item != "div"]
    item = item[item != "form-group shiny-input-container"]
  })
  uilist
})

output$plot = renderPlotly({
  current.plot$plot
})

output$radar = radarchart::renderChartJSRadar({
  current.plot$plot = createRadarPlot(current.data$data)
  current.plot$plot
})

output$newtab = renderUI({
  h4("Here you can save the plot to an extra tab if you'd like to:")
  textInput("newtabname",
            "Select the name for the new tab with this plot",
            input$plotchoice)
})
