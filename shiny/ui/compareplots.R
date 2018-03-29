tabpanel.compareplots =  list(
    conditionalPanel("output.fileUploaded == true && input.Submit == true",
        h3("Here you can compare two of your saved plots"),
        conditionalPanel("output.plotsSaved == true",
            fluidRow(
                column(6,
                    uiOutput("selectPlot1")
                ),
                column(6,
                    uiOutput("selectPlot2") 
                )
            ),
            fluidRow(
                column(6,
                    uiOutput("rerenderPlot1", width = "20%") 
                ),
                column(6,
                    uiOutput("rerenderPlot2", width = "20%")  
                )
            )                  
        ) 
    )
)