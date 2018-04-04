tabpanel.table =  list(
    conditionalPanel("output.fileUploaded == true && input.Submit == true",
        fluidRow(
            column(5, h3("Aggregate and transform your benchmark results")),
            column(3, uiOutput("help.table"))              
        ),
        fluidRow(
            column(6,
                uiOutput("table.aggregation"),
                actionButton("Aggregation", "Aggregation", icon = icon("check")),
                actionButton("Reset", "Reset", icon = icon("check"))
            ),
            column(6,
                uiOutput("table.transformation"),
                actionButton("Transformation", "Transformation", icon = icon("check")),
                actionButton("DeleteSelectedRows", "DeleteSelectedRows", icon = icon("check"))                
            )
        ),

        DT::dataTableOutput("DataTable")
    )
)
