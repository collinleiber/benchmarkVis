output$rankplot.measure.submitted = renderUI({
  req(data()) #only execute the rest, if dataframe is available
  data = data() #dataframe needs do be assigned reactively
  column(
    6,
    selectInput(
      'rp.measure.submitted',
      'performance measure to compare on',
      input$measures,
      multiple = FALSE
    )
  )
})

output$rankplot.measure = renderUI({
  req(data()) #only execute the rest, if dataframe is available
  data = data() #dataframe needs do be assigned reactively
  column(
    6,
    selectInput(
      'rank.measure',
      'performance measure to rank',
      gdata$aggcol,
      multiple = FALSE
    )
  )
})

output$plot_rank_submitted = renderPlotly({
  req(input$rp.measure.submitted)
  createRankPlot(table$data, input$rp.measure.submitted)
})

output$plot_rank_aggr = renderPlotly({
  req(input$rank.measure)
  createRankPlot(gdata$dt, input$rank.measure)
})
