require(data.table)
tabpanel.table =  list(
    conditionalPanel("output.fileUploaded == true && input.Submit == true",
        h3("Here you can aggregate and play with a table"),
        fluidRow(
            column(3,
                uiOutput("table.aggregation"),
                actionButton("Aggregation", "Aggregation", icon = icon("check")),
                actionButton("Reset", "Reset", icon = icon("check"))
            ),
            column(3,
                uiOutput("table.transformation"),
                actionButton("Transformation", "Transformation", icon = icon("check")),
                actionButton("DeleteSelectedRows", "DeleteSelectedRows", icon = icon("check"))                
            )
        ),

        DT::dataTableOutput("DataTable")
    )
)
