output$tuning.parameter = renderUI({
  req(data()) #only execute the rest, if dataframe is available
  data = data() #dataframe needs do be assigned reactively
  column(
    6,
    selectInput(
      'tun.param',
      'tuning parameter to compare on',
      input$algorithm.parameter,
      multiple = FALSE
    )
  )
})

output$tuning.measure = renderUI({
  req(data()) #only execute the rest, if dataframe is available
  data = data() #dataframe needs do be assigned reactively
  column(
    6,
    selectInput(
      'tun.measure',
      'tuning measure to compare on',
      input$measures,
      multiple = FALSE
    )
  )
})

output$plot_tun = renderPlotly({
  createTuningIterationPlot(table$data, input$tun.measure)
})
