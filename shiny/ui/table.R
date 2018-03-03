require(data.table)
tabpanel.table =  list(
     h3("Here you can aggregate and play with a table"),
              uiOutput("table.aggregation"),
              actionButton("Aggregation", "Aggregation", icon = icon("check")),
              actionButton("Reset", "Reset", icon = icon("check")),
              uiOutput("table.transformation"),
              actionButton("Transformation", "Transformation", icon = icon("check")),
              DT::dataTableOutput("DataTable")
)
