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
                conditionalPanel("input.createplot > 0",
                    uiOutput("newtab"),
                    actionButton("createtab", "Create a new tab for this plot", icon = icon("check"))
                )          
            )
        ),
        conditionalPanel("input.createplot > 0",
            plotlyOutput("plot")
        ) 
    )
)
