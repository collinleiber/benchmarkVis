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
    "rbenchmark" = useRbenchmarkFileWrapper
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
      #shinyjs::reset("cond1")
      #shinyjs::reset("cond2")
})

observeEvent(input$Submit, {
  table$data =
  {
    req(data())
    data = data()
  }
})

output$fileUploaded = reactive({
  return(!is.null(data()))
})

outputOptions(output, 'fileUploaded', suspendWhenHidden = FALSE)

output$data.format = renderUI({
  data.types = c('csv', 'microbenchmark', 'mlr', 'mlr tuning', 'rbenchmark')
  column(6,
         selectInput('dataformat', 'Choose your data type', data.types, selected = 'csv'))
})

output$accepted = renderImage({
  list(src = './images/accepted.png',
       alt = paste("Submit was successful"))

}, deleteFile = FALSE)
