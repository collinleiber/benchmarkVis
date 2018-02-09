tabpanel.plots_box =  list(
     h3("Box plot for your data"),
              uiOutput("boxplot.measure"),
              plotlyOutput("plot_box")
)