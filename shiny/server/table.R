mvalues = reactiveValues(matrix = NULL)
gdata = reactiveValues(dt = NULL, aggcol = NULL, plot = NULL)
##get aggregation function, then realize the aggregation
get_data = function() {
  groupby = isolate((input$gcolumns))
  aggfun = isolate((input$aggrf))
  aggcol = isolate((input$aggrcol))
  df = data()
  gdata$dt = get.result(groupby, aggfun, aggcol, df)
  result = gdata$dt
  gdata$aggcol = get.new.name(aggfun, aggcol)
  result
}

observeEvent(input$Aggregation, {
  mvalues$matrix =
  {
    req(data())
    data = get_data()
  }
})
observeEvent(input$Reset, {
  mvalues$matrix =
    table$data
})

observeEvent(input$rp, {
  gdata$dt = gdata$dt
})
observeEvent(input$Submit, {
  mvalues$matrix =
  {
    req(data()) #only execute the rest, if dataframe is available
    req(input$Submit) #only show the content if user has submitted
    data = data()
  }
})
output$table.aggregation = renderUI({
  req(data()) #only execute the rest, if dataframe is available
  req(input$Submit) #only show the content if user has submitted
  data = data()
  aggregationfunction_set = list("mean", "median", "standard deviation")
  list(
    selectInput(
      'gcolumns',
      'GroupBy Columns',
      colnames(data),
      selected = FALSE,
      multiple = TRUE
    ),
    selectInput(
      'aggrf',
      'Aggregation Function',
      aggregationfunction_set,
      selected = FALSE,
      multiple = TRUE
    ),
    selectInput(
      'aggrcol',
      'Aggregated Column',
      get.num.columns.name(data),
      selected = FALSE,
      multiple = TRUE
    )
  )
})

output$DataTable = DT::renderDataTable(
  mvalues$matrix,
  filter = 'top',
  extensions = c('Buttons', 'ColReorder', 'FixedColumns'),
  options = list(
    pageLength = 5,
    lengthMenu = c(5, 10, 15, 20),
    scrollX = TRUE,
    dom = 'Bfrtip',
    buttons = c(I('colvis'), 'copy', 'csv', 'excel', 'pdf', 'print'),
    colReorder = TRUE,
    dom = 't',
    fixedColumns = list(leftColumns = 1),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
      "}"
    )
  )
)
