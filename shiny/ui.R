## ui.R ##
library(shinydashboard)
library(plotly)
library(shinyjs)
library(V8)
library(shinyBS)
#needed for shinyapps.io
#library(devtools)
#devtools::install_github("collinleiber/benchmarkVis")
#library(benchmarkVis)

css.errors = "
            .shiny-output-error { visibility: hidden; }
            .shiny-output-error:before {
              visibility: visible;
              content: 'Ooops, some error occurred, you should probably restart the app'; }
            }
            "

jsResetCode = "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page                                             
ui.files = list.files(path = "./ui", pattern = "*.R")
ui.files = paste0("ui/", ui.files)

get.help = function(question, placement='bottom') {
   div(style="width: 50 px; height: 30px;",
      popify(imageOutput("help", width = "30px", height="30px"), 
        title = shiny.help[question][[1]][1],
        content = shiny.help[question][[1]][2], 
        placement = placement
      )
   )
}

for (i in seq_along(ui.files)) {
  source(ui.files[i], local = TRUE)
}

dashboardPage(
  dashboardHeader(title = "benchmarkVis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Import", tabName = "import", icon = icon("file-excel-o")),
      menuItem("Data table", tabName = "table", icon = icon("table")),
      menuItem("Plots", tabName = "plots", icon = icon("line-chart")),      
      menuItem("Saved Plots", tabName = "savedplots", icon = icon("eye ")),
      menuItem("Compare Plots", tabName = "compareplots", icon = icon("th-large"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "import", tabpanel.import),
      tabItem(tabName = "table", tabpanel.table),
      tabItem(tabName = "plots", tabpanel.plots),
      tabItem(tabName = "savedplots", tabpanel.savedplots),
      tabItem(tabName = "compareplots", tabpanel.compareplots)
    )
  )
)
