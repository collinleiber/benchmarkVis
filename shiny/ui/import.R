tabpanel.import =  list(
       h3("Import the file with benchmark results"),
              uiOutput("data.type"),
              fileInput("file", "Choose File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   ".csv",
                                   ".Rds")),
              conditionalPanel("output.fileUploaded == true && input.Submit == false",
                h4("Select the right columns:"),
                uiOutput("data.columns"),
                checkboxInput("replication", "replication", FALSE),
                conditionalPanel(
                  condition = "input.replication == true",
                  uiOutput("data.replication")
                ),
                actionButton("Submit", "Submit", icon = icon("check"))
              ),
              conditionalPanel(
                condition = "input.Submit == true",
                imageOutput("accepted")
              )
    
  
)