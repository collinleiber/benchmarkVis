library(DT)
library(plotly)

server <- function(input, output) {
  readData <- function(){
    req(input$file1)

    data <- read.csv(input$file1$datapath)
    data
  }
  output$contents <- renderTable({

    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.

    readData()


  })

  output$myDataTable <- DT::renderDataTable({
    dataframe = readData()
  },
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
                   "}")))

  output$plot <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~wt)
  })

  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })

}
