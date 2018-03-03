mvalues = reactiveValues(matrix = NULL)
gdata = reactiveValues(dt = NULL, aggcol = NULL, plot = NULL, trancol = NULL)
##get aggregation function, then realize the aggregation
get_data = function() {
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

observeEvent(input$Aggregation, {
  mvalues$matrix =
  {
    req(data())
    data = get_data()
    if (!is.null(gdata$dt)) {
      data = gdata$dt
    }
    data
  }
})

observeEvent(input$Reset, {
  gdata$trancol = NULL
  mvalues$matrix =
    table$data
})
observeEvent(input$Transformation, {
  df = isolate(gdata$dt)
  measure = isolate(input$trancols)

  if ("rank" == isolate(input$tranfuns)) {
    i = 1
    for(x in measure){
      abcd = df %>% dplyr::mutate(rank = order(eval(parse(text = sprintf("%s", x)))))

      order.scores = order(df[, x])

      #order.scores = order(df[, measure])

      rank = NA
      rank[order.scores] = seq_len(nrow(df))
      name = paste0('rank(',x,')')
      i = i + 1
      rank = as.factor(rank)

      df = cbind(df, rank)

      colnames(df)[ncol(df)] <- name

    }
  }

  #cols <- names(df) == "rank"
  #names(df)[cols] <- paste0('rank', seq_along(cols))

  mvalues$matrix = df
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

output$table.transformation = renderUI({
  req(data()) #only execute the rest, if dataframe is available
  req(input$Submit) #only show the content if user has submitted
  data = data()
  trancols = get.num.columns.name(data)
  tranfuns = list("rank")
  if (is.null(gdata$trancol)) {
  } else {
    trancols = gdata$trancol
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
      'rank etc....'
    )
  )
})

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
      'mean,sd,median etc....'
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
