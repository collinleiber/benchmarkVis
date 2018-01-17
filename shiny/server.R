library(DT)
library(plotly)

server <- function(input, output) {
  data = reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })

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


  output$myDataTable <- DT::renderDataTable(
    {req(data()) #only execute the rest, if dataframe is available
      req(input$Submit) #only show the content if user has submitted
      data = data()},
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
    plot_ly(mtcars, x = ~mpg, y = ~wt)
  })

  output$event <- renderPrint({
    req(input$Submit) #only show the content if user has submitted
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })

}
