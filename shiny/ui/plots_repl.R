tabpanel.plots_repl =  list(
    h3("Replication line plot for your data"),
              uiOutput("replication.measure"),
              plotlyOutput("plot_repl")
)