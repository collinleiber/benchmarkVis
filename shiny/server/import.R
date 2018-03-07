options(shiny.maxRequestSize = 35 * 1024 ^ 2)
table = reactiveValues(data = NULL)

data = reactive({
  if (is.null(input$file))
    return(NULL)
  wrapper = switch(
    input$datatype,
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

output$data.type = renderUI({
  data.types = c('csv', 'microbenchmark', 'mlr', 'mlr tuning', 'rbenchmark')
  column(6,
         selectInput('datatype', 'Choose your data type', data.types, selected = 'csv'))
})

output$accepted = renderImage({
  list(src = './images/accepted.png',
       alt = paste("Submit was successful"))

}, deleteFile = FALSE)

output$data.columns = renderUI({
  req(data()) #only execute the rest, if dataframe is available
  data = data() #dataframe needs do be assigned reactively
  list(
    column(
      6,
      selectInput('problem', 'problem', colnames(data), selected = 'problem')
    ),
    column(
      6,
      selectInput(
        'problem.parameters',
        'problem parameters',
        colnames(data),
        selected = 'problem.parameter'
      )
    ),
    column(
      6,
      selectInput('algorithm', 'algorithm', colnames(data), selected = 'algorithm')
    ),
    column(
      6,
      selectInput(
        'algorithm.parameters',
        'algorithm parameters',
        colnames(data),
        selected = 'algorithm.parameter'
      )
    ),
    column(
      6,
      selectInput('measures', 'measures', colnames(data), multiple = TRUE)
    )
  )
})

output$data.replication = renderUI({
  req(data()) #only execute the rest, if dataframe is available
  data = data() #dataframe needs do be assigned reactively
  list(column(
    6,
    selectInput('replication', 'replication', colnames(data), selected = 'replication')
  ),
  column(
    6,
    selectInput(
      'replication.parameters',
      'replication parameters',
      colnames(data),
      selected = 'replication.parameter'
    )
  ),
  column(
    6,
    selectInput(
      'replication.measures',
      'replication measures',
      colnames(data),
      multiple = TRUE
    )
  ))
})
