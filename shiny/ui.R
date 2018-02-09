## ui.R ##
library(shinydashboard)
library(plotly)

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
      #menuItem("Filter", tabName = "filter", icon = icon("cog")), # http://fontawesome.io/icons/ hier kann man icons finden
      menuItem("DT", tabName = "table", icon = icon("table")),
      menuItem("Box Plot", tabName = "plots_box", icon = icon("line-chart")),
      menuItem("Rank Plot", tabName = "plots_rank", icon = icon("line-chart")),
      menuItem("Replication Plot", tabName = "plots_repl", icon = icon("line-chart")),
      menuItem("Tuning Plots", tabName = "plots_tun", icon = icon("line-chart"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "import", tabpanel.import),
      #tabItem(tabName = "filter", tabpanel.filter),
      tabItem(tabName = "table", tabpanel.table),
      tabItem(tabName = "plots_box", tabpanel.plots_box ),
      tabItem(tabName = "plots_rank", tabpanel.plots_rank),
      tabItem(tabName = "plots_repl", tabpanel.plots_repl),
      tabItem(tabName = "plots_tun", tabpanel.plots_tun)
    )
  )
)
