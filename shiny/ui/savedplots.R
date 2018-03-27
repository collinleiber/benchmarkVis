tabpanel.savedplots =  list(
    conditionalPanel("output.fileUploaded == true && input.Submit == true",
        h3("Here you can have a look at your saved plots"),        
        conditionalPanel("output.plotsSaved == true",
            fluidRow(
                column(3,
                    uiOutput("savedPlotSelection")
                ),
                column(3,                
                    actionButton("deletePlot", "Delete this plot", icon = icon("check"))                        
                )
            ),
            uiOutput("rerenderPlot")                 
        )        
    )
)
 