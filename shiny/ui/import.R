tabpanel.import =  list(
       h3("Import the file with benchmark results"),              
              uiOutput("data.format"),
              fileInput("file", "Choose File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   ".csv",
                                   ".Rds"),
                        placeholder = "No file selected"),
              conditionalPanel("output.fileUploaded == true && input.Submit == false",
                actionButton("Submit", "Submit", icon = icon("check"))
              ),
              conditionalPanel(
                condition = "input.Submit == true",                
                imageOutput("accepted")
                #,
                #actionButton("new.data", "Upload new data", icon = icon("check"))
              )
)