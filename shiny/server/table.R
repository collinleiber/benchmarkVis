mvalues = reactiveValues(matrix = NULL)
gdata = reactiveValues(dt = NULL, aggcol = NULL, plot = NULL, trancol = NULL)

observeEvent(input$Submit, {
    req(data()) #only execute the rest, if dataframe is available
    req(input$Submit) #only show the content if user has submitted
    mvalues$matrix = data()
})

observeEvent(input$Reset, {
  gdata$trancol = NULL
  mvalues$matrix = table$data
})

observeEvent(input$Aggregation, {
    req(data())    
    if (!is.null(gdata$dt)) {
      mvalues$matrix = gdata$dt
    }
    else {
      mvalues$matrix = get.aggr.data()
    }
})

get.aggr.data = function() {
  groupby = isolate((input$gcolumns))
  aggfun_list = isolate((input$aggrf))
  aggcol = isolate((input$aggrcol))
  df = data()
  aggfun <- parser.agg.input(aggfun_list)
  gdata$dt = get.result(groupby, aggfun, aggcol, df)
  gdata$trancol = get.num.columns.name(gdata$dt)
  gdata$aggcol = gdata$trancol
  result = gdata$dt
  result
}

output$table.aggregation = renderUI({
  req(data()) #only execute the rest, if dataframe is available
  req(input$Submit) #only show the content if user has submitted
  data = data()
  list(
    selectInput(
      'gcolumns',
      'GroupBy Columns',
      colnames(data),
      selected = FALSE,
      multiple = TRUE
    ),
    textInput(
      'aggrf',
      'Aggregation Function',
      ''
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

observeEvent(input$Transformation, {
  df = isolate(gdata$dt)
  measure = isolate(input$trancols)

  if ("rank" == isolate(input$tranfuns)) {
    for(x in measure){
      abcd = df %>% dplyr::mutate(rank = order(eval(parse(text = sprintf("%s", x)))))

      order.scores = order(df[, x])

      rank = NA
      rank[order.scores] = seq_len(nrow(df))
      name = paste0('rank(',x,')')
      rank = as.factor(rank)

      df = cbind(df, rank)

      colnames(df)[ncol(df)] <- name
    }
  }

  #cols <- names(df) == "rank"
  #names(df)[cols] <- paste0('rank', seq_along(cols))

  mvalues$matrix = df
})

get.transform.data = function() {
  groupby = isolate((input$gcolumns))
  aggfun_list = isolate((input$aggrf))
  aggcol = isolate((input$aggrcol))
  df = data()
  aggfun <- parser.agg.input(aggfun_list)
  gdata$dt = get.result(groupby, aggfun, aggcol, df)
  gdata$trancol = get.num.columns.name(gdata$dt)
  gdata$aggcol = gdata$trancol
  result = gdata$dt
  result
}

output$table.transformation = renderUI({
  req(data()) #only execute the rest, if dataframe is available
  req(input$Submit) #only show the content if user has submitted
  data = data()
  if (!is.null(gdata$trancol)) {
    trancols = gdata$trancol
  }
  else {    
    trancols = get.num.columns.name(data)
  }
  list(
    selectInput(
      'trancols',
      'Transformation Columns',
      trancols,
      selected = FALSE,
      multiple = TRUE
    ),
    textInput(
      'tranfuns',
      'Transformation Functions',
      ''
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
    buttons = c(I('colvis'), 'copy', 'csv', 'pdf', 'print'),
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
