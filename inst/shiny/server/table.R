mvalues = reactiveValues(matrix = NULL)
aggregated.data = reactiveValues(
  dt = NULL,
  aggcol = NULL,
  plot = NULL,
  trancol = NULL,
  rows.selected = NULL
)

observeEvent(input$Submit, {
  req(data()) #only execute the rest, if dataframe is available
  req(input$Submit) #only show the content if user has submitted
  aggregated.data$dt = tableTransformationExport(table$data, TRUE)
})

observeEvent(input$Reset, {
  aggregated.data$dt = tableTransformationExport(table$data, TRUE)
})

observeEvent(input$Aggregation, {
  req(data())
  aggregated.data$dt = get.aggr.data()
})

observeEvent(input$DataTable_rows_selected, {
  aggregated.data$rows.selected = input$DataTable_rows_selected
})

observeEvent(input$DeleteSelectedRows, {
  selected.rows = isolate(aggregated.data$rows.selected)
  req(data())
  if (!is.null(selected.rows)) {
    aggregated.data$dt = aggregated.data$dt[-as.numeric(selected.rows), ]
  }
})

get.aggr.data = function() {
  groupby = isolate(input$gcolumns)
  aggfun.list = isolate(input$aggrf)
  aggcol = isolate(input$aggrcol)
  aggfun = parser.function.list(aggfun.list)
  result = tryCatch(aggregation.apply(groupby, aggfun, aggcol, aggregated.data$dt), error = function(e) FALSE)
  # Catch error from aggregation -> return original table
  if (is.logical(result) && result == FALSE) {
    return(aggregated.data$dt)
  }
  return(result)
}

output$table.aggregation = renderUI({
  req(data()) #only execute the rest, if dataframe is available
  req(input$Submit) #only show the content if user has submitted
  list(
    selectInput(
      "gcolumns",
      "GroupBy Columns",
      getMainColumns(aggregated.data$dt),
      selected = FALSE,
      multiple = TRUE
    ),
    textInput("aggrf",
              "Aggregation Function",
              ""),
    selectInput(
      "aggrcol",
      "Aggregated Column",
      getMeasures(aggregated.data$dt),
      selected = FALSE,
      multiple = TRUE
    )
  )
})

observeEvent(input$Transformation, {
  aggregated.data$dt = get.transform.data()
})

get.transform.data = function() {
  original.data = tableTransformationImport(isolate(aggregated.data$dt), TRUE)
  columns.to.transform = isolate(input$trancols)
  transformation.functions = isolate(input$tranfuns)
  transformation.functions = parser.function.list(transformation.functions)
  result = tryCatch(transformation.apply(original.data,
                                columns.to.transform,
                                transformation.functions), error = function(e) FALSE)
  # Catch error from transformation -> return original table
  if (is.logical(result) && result == FALSE) {
    return(aggregated.data$dt)
  }
  return(tableTransformationExport(result, TRUE))
}

output$table.transformation = renderUI({
  list(
    selectInput(
      "trancols",
      "Transformation Columns",
      c(getMeasures(aggregated.data$dt), getLists(aggregated.data$dt)),
      selected = FALSE,
      multiple = TRUE
    ),
    textInput("tranfuns",
              "Transformation Functions",
              "")
  )
})


output$DataTable = DT::renderDataTable(
  aggregated.data$dt,
  filter = "top",
  extensions = c("Buttons", "ColReorder", "FixedColumns"),
  rownames = FALSE,
  options = list(
    pageLength = 10,
    #lengthMenu = c(5, 10, 15, 20),
    scrollX = TRUE,
    dom = "Blfrtip",
    buttons = c(I("colvis"), "copy", "csv"),
    colReorder = TRUE,
    dom = "t",
    fixedColumns = list(leftColumns = 1),
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#003366', 'color': '#fff'});",
      "}"
    )
  )
)
