tabpanel.import =  list(
    shinyjs::useShinyjs(), 
     extendShinyjs(text = jsResetCode),
       h3("Import the file with benchmark results"),    
       div(id="importtab",    
              uiOutput("data.format"),
              fileInput("file", "Choose File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   ".csv",
                                   ".Rds",
                                   ".json"),
                        placeholder = "No file selected"),
              conditionalPanel("output.fileUploaded == true && input.Submit == false",
                actionButton("Submit", "Submit", icon = icon("check"))
              ),
              conditionalPanel(
                condition = "input.Submit == true",     
                actionButton("new.data", "Upload new data", icon = icon("sync")),
                br(),br(),
                imageOutput("accepted")                
              )
       )
)