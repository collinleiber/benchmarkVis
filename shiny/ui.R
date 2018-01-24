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
      menuItem("Box Plot", tabName = "plots_box", icon = icon("line-chart")),
      menuItem("Rank Plot", tabName = "plots_rank", icon = icon("line-chart")),
      menuItem("Replication Plot", tabName = "plots_repl", icon = icon("line-chart"))

    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "import",
              h3("Import the file with benchmark results"),
              fileInput("file1", "Choose File (CSV or RData are supported by now)",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv",
                                   "application/RData",
                                   ".RData")),
              h4("Select the right columns:"),
              uiOutput("data.columns"),
              checkboxInput("replication", "replication", FALSE),
              conditionalPanel(
                condition = "input.replication == true",
                uiOutput("data.replication")
              ),
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

      tabItem(tabName = "plots_box",
              h3("Box plot for your data"),
              uiOutput("boxplot.measure"),
              plotlyOutput("plot_box")
      ),
      tabItem(tabName = "plots_rank",
              h3("Rank plot for your data"),
              plotlyOutput("plot_rank")
      ),
      tabItem(tabName = "plots_repl",
              h3("Replication line plot for your data"),
              uiOutput("replication.measure"),
              plotlyOutput("plot_repl")
      )



    )

  )
)
