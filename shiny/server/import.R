options(shiny.maxRequestSize = 35 * 1024 ^ 2)
table = reactiveValues(data = NULL)

data = reactive({
  if (is.null(input$file))
    return(NULL)
  wrapper = switch(
    input$dataformat,
    "csv" = csvImport,
    "microbenchmark" = useMicrobenchmarkFileWrapper,
    "mlr" = useMlrBenchmarkFileWrapper,
    "mlr tuning" = useMlrTuningFileWrapper,
    "rbenchmark" = useRbenchmarkFileWrapper,
    "json" = jsonImport
  )
  dt = wrapper(input$file$datapath)
  if (checkStructure(dt)) {
    dt
  }
  else{
    return(NULL)
  }
})

observeEvent(input$new.data, {
      shinyjs::reset("file")
      shinyjs::reset("importtab")
})

observeEvent(input$Submit, {
  table$data = data()  
  current.data$data = table$data
  aggregated.data$dt = table$data
})

output$fileUploaded = reactive({
  return(!is.null(data()))
})

outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)

output$data.format = renderUI({
  data.types = c('csv', 'microbenchmark', 'mlr', 'mlr tuning', 'rbenchmark', 'json')
  column(6,
         selectInput('dataformat', 'Choose your data type', data.types, selected = 'csv'))
})

output$accepted = renderImage({
  list(src = './images/accepted.png',
       alt = paste("Submit was successful"))
}, deleteFile = FALSE)

output$help = renderImage({
  style="width: 30 px; height: 30px;"
  list(src = './images/help.svg', width="30px", height="30px")
}, deleteFile = FALSE)