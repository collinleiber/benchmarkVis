library(DT)
server <- function(input, output) {

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
      data =get_data()}
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

  #definition of aggregation function
  do_agg <- function(fun_str) {
    groupby <- isolate((input$gcolumns))
    aggfun <- isolate((input$aggrf))
    aggcol <- isolate((input$aggrcol))
    tmp = data()
    tag = TRUE
    check_data_type <- function(type){
      tag <<- tag && type == "numeric"
    }
    types = lapply(tmp[, aggcol], class)
    lapply(types, check_data_type)
    if (tag) {
      if (fun_str == "standard deviation") {
        func = stats::sd
      } else if (fun_str == "mean") {
        func = mean
      } else if (fun_str == "median") {
        func = median
      } else {
        #handle err
      }
    }else {
    #   #err msg
      warning("invaild type to aggregate")
       showNotification("invaild type to aggregate", type = "error")
       #validate(need(!tag, "invaild type to aggregate."))
       tmp
     }
      newtable <- aggregate(x = tmp[c(aggcol)],
                            by = tmp[c(groupby)],
                            FUN = func)
                            return(newtable)
    }

      #rename the aggregated columns
    get_newname <- function(fun_str){
      groupby <- isolate((input$gcolumns))
      aggfun <- isolate((input$aggrf))
      aggcol <- isolate((input$aggrcol))
      newtable = do_agg(fun_str)
      newtable = newtable[, aggcol, drop = FALSE]
      newcolsname = lapply(aggcol,
                           FUN = function(colname) {
                             newname = paste(fun_str,"_", colname, "", sep  = "")
                           }
      )
      gdata$aggcol = newcolsname
      colnames(newtable) <- newcolsname
      return(newtable)

  }
  ##get aggregation function, then realize the aggregation
  get_data <- function() {
    input$Aggregation
    # result = (data())
    groupby <- isolate((input$gcolumns))
    aggfun <- isolate((input$aggrf))
    aggcol <- isolate((input$aggrcol))
     result = do_agg("mean")
    result=result[,groupby, drop = FALSE]
    #result = result[, groupby, drop = FALSE]
    if (is.element("mean", aggfun)) {
      newtable <- get_newname("mean")
      result = cbind(result, newtable)
    }

    if (is.element("standard deviation", aggfun)) {
      newtable <- get_newname("standard deviation")
      result = cbind(result, newtable)
    }
    if (is.element("median", aggfun)) {
      newtable <- get_newname("median")
      result = cbind(result, newtable)
    }
    gdata$dt <- result
    if (length(aggcol) >= 2) {
      gdata$problem <- aggcol[1]
      gdata$algo <- aggcol[2]
    }
    result
  }

  #only nummeric columns can be aggregated
  get_num_columns_name <- function(data) {
    colnames <- list()
    for (col_name in names(data))
    {
      if (is.numeric(data[,col_name]))
      {
        colnames<-c(colnames, col_name)
      }
    }
    colnames
  }

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
      selectInput('aggrcol', 'Aggregated Column', get_num_columns_name(data), selected = FALSE, multiple = TRUE)
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
    req(input$rank.measure)
    createRankPlot(table$data, input$rp.measure.submitted, input$problem, input$algorithm)

  })

  output$plot_rank_aggr <- renderPlotly({
    req(input$rank.measure)
    createRankPlot(gdata$dt, input$rank.measure, gdata$problem, gdata$algo)

  })

  output$plot_repl <- renderPlotly({
    req(input$replication)
    req(input$replication.measure)
    createReplicationLinePlot(table$data, input$replication.measure)
  })

  output$plot_tun <- renderPlotly({
    createTuningParameterPlot(table$data, input$tun.param, input$tun.measure)
  })


}
