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
  
  output$myDataTable <- DT::renderDataTable(readData())
  
  output$plot <- renderPlotly({
    plot_ly(mtcars, x = ~mpg, y = ~wt)
  })
  
  output$event <- renderPrint({
    d <- event_data("plotly_hover")
    if (is.null(d)) "Hover on a point!" else d
  })

}
