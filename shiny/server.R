server <- function(input, output) {

  mvalues <- reactiveValues(matrix = NULL)
  gdata <- reactiveValues(dt = NULL, aggcol = NULL, plot = NULL)

  data <- reactive({
    req(input$file1)
    print(input$file1)
    if (input$file1$type=="text/csv"){
      read.csv(input$file1$datapath)
    }
    else if (endsWith(input$file1$name,".RData")){
      get(load(input$file1$datapath))
    }
  })

  table <- reactiveValues(data = NULL)

  observeEvent(input$Submit,{
    table$data <-
    {req(data())
      data = data()}
  })

  observe({
    if (input$Aggregation == 0)
      return()
    mvalues$matrix <- table$data
  })
  observe({
    if (input$Reset == 0)
      return()
    mvalues$matrix <- table$data
  })
  observeEvent(input$Submit,{
      mvalues$matrix <-
      {req(data()) #only execute the rest, if dataframe is available
        req(input$Submit) #only show the content if user has submitted
        data = data()}
  })
  observeEvent(input$Submit,{
    aggcol <- gdata$aggcol
    data <- gdata$dt
    if (length(aggcol)) {
      first_agg_col = aggcol[[1]]
      order.scores<-order(data[[first_agg_col]])
      data$rank <- NA
      data$rank[order.scores] <- 1:nrow(data)
      problem = input$dc
      p = ggplot(data, aes_string("rank", problem, fill = input$lc))+
        geom_tile(position = "dodge")+
        labs(title= "Rank Plot",
             x= "Rank",
             y= "Task.id")
      gdata$plot <- p
    }
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

      #rename the aggregated columns
      newtable <- aggregate(x = tmp[c(aggcol)],
                            by = tmp[c(groupby)],
                            FUN = func)
      newtable = newtable[, aggcol, drop = FALSE]
      newcolsname = lapply(aggcol,
                           FUN = function(colname) {
                             newname = paste(fun_str,"_", colname, "", sep  = "")
                           }
      )
      gdata$aggcol = newcolsname
      colnames(newtable) <- newcolsname
      newtable
    } else {
      #err msg
      warning("invaild type to aggregate")
      showNotification("invaild type to aggregate", type = "error")
      #validate(need(!tag, "invaild type to aggregate."))
      tmp
    }

  }
  ##get aggregation function, then realize the aggregation
  get_data <- function() {
    input$Aggregation
    result = (data())
    groupby <- isolate((input$gcolumns))
    aggfun <- isolate((input$aggrf))
    aggcol <- isolate((input$aggrcol))
    result = result[, groupby, drop = FALSE]
    if (is.element("mean", aggfun)) {
      newtable = do_agg("mean")
      result = cbind(result, newtable)
    }
    if (is.element("standard deviation", aggfun)) {
      newtable = do_agg("standard deviation")
      result = cbind(result, newtable)
    }
    if (is.element("median", aggfun)) {
      newtable = do_agg("median")
      result = cbind(result, newtable)
    }
    gdata$dt <- result
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

  output$contents <- renderTable({
    table$data
  })


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

  output$replication.measure = renderUI({
    req(data()) #only execute the rest, if dataframe is available
    data = data() #dataframe needs do be assigned reactively
    column(6, selectInput('replication.measure', 'replication measure to compare on', input$replication.measures, multiple = FALSE))
  })

  output$filter <- DT::renderDataTable(
    {req(data()) #only execute the rest, if dataframe is available
      req(input$Submit) #only show the content if user has submitted
      data = data()},
    filter = 'top',
    extensions = c('Buttons', 'RowReorder'),
    options = list(pageLength = 5,
                   lengthMenu = c(5, 10, 15, 20),
                   scrollX = TRUE, dom = 'Bfrtip', buttons = c( I('colvis')),
                   dom = 't',
                   rowReorder = TRUE)
  )


  output$table.aggregation =renderUI({
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

  output$plot_rank <- renderPlotly({

    gdata$plot

  })

  output$plot_repl <- renderPlotly({
    req(input$replication)
    req(input$replication.measure)
    createReplicationLinePlot(table$data, input$replication.measure)
  })

}
