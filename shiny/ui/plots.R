tabpanel.plots =  list(
    conditionalPanel("output.fileUploaded == true && input.Submit == true",
        h3("Here you can create plots for your data"),
        fluidRow(
            column(4,
                radioButtons("current.data", "Choose type of data to work with:",
                           choices = c('submitted data', 'aggregated data'),
                           selected = "submitted data"),
                uiOutput("plotselection")
            ),
            column(4,
                uiOutput("plot.parameter.selection"),
                actionButton("createplot", "Create plot", icon = icon("check"))          
            ),
            column(3,
                conditionalPanel("input.createplot == true",
                    actionButton("create.tab", "Create a new tab for this plot", icon = icon("check"))
                )          
            )
        ),
        #TODO: conditional panel with plot
        conditionalPanel("input.createplot == true",
            plotlyOutput("plot")
        ) 
    )
)
