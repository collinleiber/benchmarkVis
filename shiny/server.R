library(DT)
library(plotly)
library(ggplot2)


server <- function(input, output) {

  mvalues <- reactiveValues(matrix = NULL)
  gdata <- reactiveValues(dt = NULL, aggcol = NULL, plot = NULL)

  data = reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })

  ##definition of aggregation function
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

      ##rename the aggregated columns
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

  ##only nummeric columns can be aggregated
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

  #data = readData()
  output$contents <- renderTable({
    req(data()) #only execute the rest, if dataframe is available
    req(input$Submit) #only show the content if user has submitted
    data = data()
    data
  })


  output$data.columns = renderUI({
    req(data()) #only execute the rest, if dataframe is available
    data = data() #dataframe needs do be assigned reactively
    list(
      column(6, selectInput('dc', 'problem', colnames(data), selected = FALSE)),
      column(6, selectInput('lc', 'algorithm', colnames(data), selected = FALSE)),
      column(6, selectInput('pc', 'measures', colnames(data), multiple = TRUE))
    )

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

  observe({
    if (input$Aggregation == 0)
      return()
    mvalues$matrix <-
      {req(data()) #only execute the rest, if dataframe is available
      req(input$Submit) #only show the content if user has submitted
      data = get_data()}
  })
  observe({
    if (input$Reset == 0)
      return()
    mvalues$matrix <-
      mvalues$matrix <-
      {req(data()) #only execute the rest, if dataframe is available
        req(input$Submit) #only show the content if user has submitted
        data = data()}
  })
  observe({
    if (input$Submit == 0)
      return()
    mvalues$matrix <-
      mvalues$matrix <-
      {req(data()) #only execute the rest, if dataframe is available
        req(input$Submit) #only show the content if user has submitted
        data = data()}
  })
  observe({
    if (input$Rankplot == 0)
      return()
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

  output$myDataTable <- DT::renderDataTable(
    #{req(data()) #only execute the rest, if dataframe is available
    #  req(input$Submit) #only show the content if user has submitted
    #  data = get_data()},
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

  output$plot <- renderPlotly({
    req(input$Submit) #only show the content if user has submitted
    req(input$Aggregation)#only show the content if user has aggregated
    if (is.null(gdata$dt)){
      return()
    } else {
      gdata$plot
    }
    #plot_ly(mtcars, x = ~mpg, y = ~wt)
  })

  output$event <- renderPrint({
    req(input$Submit) #only show the content if user has submitted
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })
}
