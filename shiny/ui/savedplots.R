tabpanel.savedplots =  list(
    conditionalPanel("output.fileUploaded == true && input.Submit == true",
        h3("Here you can have a look at any of your saved plots"),        
        conditionalPanel("output.plotsSaved == true",
            uiOutput("savedPlotSelection"),
            uiOutput("rerenderPlot")                 
        )        
    )
)
 