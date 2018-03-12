output$boxplot.measure = renderUI({
  req(data()) #only execute the rest, if dataframe is available
  data = data() #dataframe needs do be assigned reactively
  column(
    6,
    selectInput(
      'box.measure',
      'performance measure to compare on',
      getMeasures(data),
      multiple = FALSE
    )
  )
})
output$plot_box = renderPlotly({
  req(input$box.measure)
  createBoxPlot(table$data, input$box.measure)
})
