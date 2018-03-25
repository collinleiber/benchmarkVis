## ui.R ##
library(shinydashboard)
library(plotly)

jsResetCode = "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page                                             
ui.files = list.files(path = "./ui", pattern = "*.R")
ui.files = paste0("ui/", ui.files)

for (i in seq_along(ui.files)) {
  source(ui.files[i], local = TRUE)
}

dashboardPage(
  dashboardHeader(title = "Benchmark Visualization"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import", tabName = "import", icon = icon("file-excel-o")),
      menuItem("Data table", tabName = "table", icon = icon("table")),
      menuItem("Plots", tabName = "plots", icon = icon("line-chart"))      
    )
     
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "import", tabpanel.import),
      tabItem(tabName = "table", tabpanel.table),
      tabItem(tabName = "plots", tabpanel.plots)
    )
  )
)
