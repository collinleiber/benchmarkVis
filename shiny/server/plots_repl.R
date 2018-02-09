output$replication.measure = renderUI({
  req(data()) #only execute the rest, if dataframe is available
  data = data() #dataframe needs do be assigned reactively
  column(
    6,
    selectInput(
      'replication.measure',
      'replication measure to compare on',
      input$replication.measures,
      multiple = FALSE
    )
  )
})

output$plot_repl = renderPlotly({
  req(input$replication)
  req(input$replication.measure)
  createReplicationLinePlot(table$data, input$replication.measure)
})
