#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## ui.R ##
library(shinydashboard)
library(plotly)

dashboardPage(
  dashboardHeader(title = "Benchmark Visualization"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import", tabName = "import", icon = icon("file-excel-o")),
      menuItem("Filter", tabName = "filter", icon = icon("cog")), # http://fontawesome.io/icons/ hier kann man icons finden
      menuItem("DT", tabName = "table", icon = icon("table")),
      menuItem("Plots", tabName = "plots", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "import",
              h3("Import the file with benchmark results"),
              fileInput("file1", "Choose CSV File",
                        multiple = TRUE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),

              #column(width = 4,
              #       box(
              #         title = "Your benchmark results", width = NULL, status = "primary",
              #         div(style = 'overflow-x: scroll', tableOutput("contents"))
              #       )
              #),
              h4("Select the right columns:"),
              #actionButton("do", "Click Me"),
              uiOutput("data.columns"),

              actionButton("Submit", "Submit", icon = icon("check")),
              tableOutput("contents")
      ),

      tabItem(tabName = "filter",
              h3("Choose the information from your results you want to visualize"),
              DT::dataTableOutput("filter")
      ),

      tabItem(tabName = "table",
              h3("Here you can aggregate and play with a table"),
              uiOutput("table.aggregation"),
              actionButton("Aggregation", "Aggregation", icon = icon("check")),
              actionButton("Reset", "Reset", icon = icon("check")),

              DT::dataTableOutput("myDataTable")
      ),

      tabItem(tabName = "plots",
              h3("Your benchmark results visualized"),
              actionButton("Rankplot","Rankplot",icon = icon("check")),
              plotlyOutput("plot"),
              verbatimTextOutput("event")
      )

    )

  )
)
