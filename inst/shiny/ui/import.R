tabpanel.import =  list(
  fluidRow(column(
    5, h3("Import the file with benchmark results")
  ),
  column(3, uiOutput("help.import"))),
  div(
    id = "importtab",
    uiOutput("data.format", style = "min-width: 100%"),
    fileInput(
      "file",
      "Choose File",
      multiple = FALSE,
      accept = c("text/csv",
                 ".csv",
                 ".rds",
                 ".json"),
      placeholder = "No file selected"
    ),
    conditionalPanel(
      "output.fileUploaded == true && input.Submit == false",
      actionButton("Submit", "Submit", icon = icon("check"))
    ),
    conditionalPanel(condition = "input.Submit == true",
                     fluidRow(
                       column(2, imageOutput("accepted")),
                       column(10, actionButton("new.data", "Upload new data"))
                     ))
  )
)
