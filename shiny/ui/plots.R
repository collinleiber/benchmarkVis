tabpanel.plots =  list(
    conditionalPanel("output.fileUploaded == true && input.Submit == true",
        h3("Here you can create plots for your data"),
        fluidRow(
            column(4,
                radioButtons("currentdata", "Choose type of data to work with:",
                           choices = c('submitted data', 'aggregated data'),
                           selected = "submitted data"),
                uiOutput("plotselection")
            ),
            column(4,
                conditionalPanel("input.plots!=0",
                box("TODO: select plot specific parameters (=what to plot)")
                    #getPlotParameters(input$plots)                           
                ),
                actionButton("createplot", "Create plot", icon = icon("check"))          
            ),
            column(3,
                conditionalPanel("input.plots != 0 && input.createplot != 0",
                    actionButton("create.tab", "Create a new tab for this plot", icon = icon("check"))
                )          
            )
        )
        #TODO: conditional panel with plot
    )
)
