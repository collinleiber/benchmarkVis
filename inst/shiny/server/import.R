options(shiny.maxRequestSize = 35 * 1024 ^ 2)
table = reactiveValues(data = NULL)

data = reactive({
  if (is.null(input$file))
    return(NULL)
  wrapper = switch(
    input$dataformat,
    "csv" = csvImport,
    "rds" = rdsImport,
    "json" = jsonImport,
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
  session$reload()
  #shinyjs::reset("importtab")
  js$reset()
})

observeEvent(input$Submit, {
  table$data = data()
  current.data$data = table$data
  aggregated.data$dt = table$data
})

output$fileUploaded = reactive({
  return(!is.null(data()))
})

outputOptions(output, "fileUploaded", suspendWhenHidden = FALSE)

output$data.format = renderUI({
  data.types = c("csv",
                 "rds",
                 "json",
                 "microbenchmark",
                 "mlr",
                 "mlr tuning",
                 "rbenchmark")
  column(6,
         selectInput("dataformat", "Choose your data type", data.types, selected = "csv"))
})

output$accepted = renderImage({
  list(
    src = "./www/accepted.png",
    width = "50px",
    height = "50px",
    alt = paste("Submit was successful")
  )
}, deleteFile = FALSE)
