tabpanel.savedplots =  list(
    conditionalPanel("output.fileUploaded == true && input.Submit == true",
        fluidRow(
            column(5, h3("Here you can find your saved plots")),
            column(3, uiOutput("help.saved.plots"))              
        ),  
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
 