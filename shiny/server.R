library(DT)
server <- function(input, output) {
  options(shiny.maxRequestSize = 35*1024^2) # set maximal size for file input
  mvalues <- reactiveValues(matrix = NULL)
  gdata <- reactiveValues(dt = NULL, aggcol = NULL, plot = NULL)
  table <- reactiveValues(data = NULL)

  data <- reactive({
    if(is.null(input$file)) return(NULL)
    wrapper <- switch(input$datatype,
      "csv" = csvImport,
      "microbenchmark" = useMicrobenchmarkFileWrapper,
      "mlr" = useMlrBenchmarkFileWrapper,
      "mlr tuning" = useMlrTuningFileWrapper,
      "rbenchmark" = useRbenchmarkFileWrapper
    )
    df = wrapper(input$file$datapath)
    if (checkStructure(df)) {
      df
    }
    else{
      return(NULL)
    }
  })

  ##get aggregation function, then realize the aggregation
  get_data <- function() {
    groupby <- isolate((input$gcolumns))
    aggfun <- isolate((input$aggrf))
    aggcol <- isolate((input$aggrcol))
    df = data()
    gdata$dt <- get.result(groupby, aggfun, aggcol, df)
    result <- gdata$dt
    gdata$aggcol = get.new.name(aggfun, aggcol)
    result
  }

  output$fileUploaded <- reactive({
    return(!is.null(data()))
  })

  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)

  observeEvent(input$Submit,{
    table$data <-
    {req(data())
      data = data()}
  })

  observeEvent(input$Aggregation,{
    mvalues$matrix <-
    {req(data())
      data = get_data()}
  })
  observeEvent(input$Reset,{
    mvalues$matrix <-
      table$data
      })

  observeEvent(input$rp,{
    gdata$dt <- gdata$dt
  })
  observeEvent(input$Submit,{
      mvalues$matrix <-
      {req(data()) #only execute the rest, if dataframe is available
        req(input$Submit) #only show the content if user has submitted
        data = data()}
  })

  output$data.type <- renderUI({
    data.types = c('csv','microbenchmark','mlr','mlr tuning','rbenchmark')
    column(6, selectInput('datatype', 'Choose your data type', data.types, selected = 'csv'))
  })

  output$accepted <- renderImage({
    list(src = './images/accepted.png',
         alt = paste("Submit was successful"))

  }, deleteFile = FALSE
  )

  output$data.columns = renderUI({
    req(data()) #only execute the rest, if dataframe is available
    data = data() #dataframe needs do be assigned reactively
    list(
      column(6, selectInput('problem', 'problem', colnames(data), selected = 'problem')),
      column(6, selectInput('problem.parameters', 'problem parameters', colnames(data), selected = 'problem.parameter')),
      column(6, selectInput('algorithm', 'algorithm', colnames(data), selected = 'algorithm')),
      column(6, selectInput('algorithm.parameters', 'algorithm parameters', colnames(data), selected = 'algorithm.parameter')),
      column(6, selectInput('measures', 'measures', colnames(data), multiple = TRUE))
    )
  })
  output$data.replication = renderUI({
    req(data()) #only execute the rest, if dataframe is available
    data = data() #dataframe needs do be assigned reactively
    list(
      column(6, selectInput('replication', 'replication', colnames(data), selected = 'replication')),
      column(6, selectInput('replication.parameters', 'replication parameters', colnames(data), selected = 'replication.parameter')),
      column(6, selectInput('replication.measures', 'replication measures', colnames(data), multiple = TRUE))
    )
  })

  output$boxplot.measure = renderUI({
    req(data()) #only execute the rest, if dataframe is available
    data = data() #dataframe needs do be assigned reactively
    column(6, selectInput('box.measure', 'performance measure to compare on', input$measures, multiple = FALSE))
  })

  output$rankplot.measure.submitted = renderUI({
    req(data()) #only execute the rest, if dataframe is available
    data = data() #dataframe needs do be assigned reactively
    column(6, selectInput('rp.measure.submitted', 'performance measure to compare on', input$measures, multiple = FALSE))
  })

  output$rankplot.measure = renderUI({
    req(data()) #only execute the rest, if dataframe is available
    data = data() #dataframe needs do be assigned reactively
    column(6, selectInput('rank.measure', 'performance measure to rank', gdata$aggcol, multiple = FALSE))
  })

  output$replication.measure = renderUI({
    req(data()) #only execute the rest, if dataframe is available
    data = data() #dataframe needs do be assigned reactively
    column(6, selectInput('replication.measure', 'replication measure to compare on', input$replication.measures, multiple = FALSE))
  })

  output$tuning.parameter = renderUI({
    req(data()) #only execute the rest, if dataframe is available
    data = data() #dataframe needs do be assigned reactively
    column(6, selectInput('tun.param', 'tuning parameter to compare on', input$algorithm.parameter, multiple = FALSE))
  })

  output$tuning.measure = renderUI({
    req(data()) #only execute the rest, if dataframe is available
    data = data() #dataframe needs do be assigned reactively
    column(6, selectInput('tun.measure', 'tuning measure to compare on', input$measures, multiple = FALSE))
  })


  output$filter <- DT::renderDataTable(
    {table$data},
    filter = 'top',
    options = list(pageLength = 5,
                   lengthMenu = c(5, 10, 15, 20),
                   scrollX = TRUE,
                   dom = 't',
                   rowReorder = FALSE)
  )


  output$table.aggregation = renderUI({
    req(data()) #only execute the rest, if dataframe is available
    req(input$Submit) #only show the content if user has submitted
    data = data()
    aggregationfunction_set <- list("mean", "median", "standard deviation")
    list(
      selectInput('gcolumns', 'GroupBy Columns', colnames(data), selected = FALSE, multiple = TRUE),
      selectInput('aggrf', 'Aggregation Function',aggregationfunction_set, selected = FALSE, multiple = TRUE),
      selectInput('aggrcol', 'Aggregated Column', get.num.columns.name(data), selected = FALSE, multiple = TRUE)
    )
  }
  )

  output$DataTable <- DT::renderDataTable(
    mvalues$matrix,
    filter = 'top',
    extensions = c('Buttons', 'ColReorder', 'FixedColumns'),
    options = list(pageLength = 5,
                   lengthMenu = c(5, 10, 15, 20),
                   scrollX = TRUE, dom = 'Bfrtip', buttons = c( I('colvis'),'copy', 'csv', 'excel', 'pdf', 'print'),
                   colReorder=TRUE,
                   dom = 't', fixedColumns = list(leftColumns = 1),
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                     "}"))
  )

  output$plot_box <- renderPlotly({
    req(input$box.measure)
    createBoxPlot(table$data, input$box.measure)
  })

  output$plot_rank_submitted <- renderPlotly({
    req(input$rp.measure.submitted)
    createRankPlot(table$data, input$rp.measure.submitted)
  })

  output$plot_rank_aggr <- renderPlotly({
    req(input$rank.measure)
    createRankPlot((gdata$dt), input$rank.measure)

  })

  output$plot_repl <- renderPlotly({
    req(input$replication)
    req(input$replication.measure)
    createReplicationLinePlot(table$data, input$replication.measure)
  })

  output$plot_tun <- renderPlotly({
    createTuningIterationPlot(table$data, input$tun.measure)
  })


}
