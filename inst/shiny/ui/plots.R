tabpanel.plots = list(
  conditionalPanel(
    "output.fileUploaded == true && input.Submit == true",
    fluidRow(column(
      5, h3("Create plots for your benchmark results")
    ),
    column(3, uiOutput("help.plots"))),
    fluidRow(
      column(
        4,
        radioButtons(
          "current.data",
          "Choose type of data to work with:",
          choices = c("original data", "aggregated data"),
          selected = "original data"
        ),
        uiOutput("plotselection")
      ),
      conditionalPanel(
        "output.plotSelected == true",
        column(
          4,
          h4("Choose parameters for the plot"),
          uiOutput("plot.parameter.selection"),
          actionButton("createplot", "Create plot", icon = icon("check"))
        ),
        column(
          4,
          uiOutput("newtab"),
          actionButton("createtab", "Save this plot in 'Saved Plots'", icon = icon("check"))
        )
      )
    ),
    conditionalPanel(
      "input.createplot > 0 && output.plotSelected == true && output.plotToRender == true",
      conditionalPanel(
        "input.plotchoice == 'Measure: Radar Plot'",
        radarchart::chartJSRadarOutput("radar", width = "450", height = "300")
      ),
      conditionalPanel(
        "input.plotchoice != 'Measure: Radar Plot'",
        plotlyOutput("plot")
      )
    )
  )
)
