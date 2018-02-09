tabpanel.plots_rank =  list(
    h3("Rank plot for your data"),
              radioButtons("rp", "Choose type of rank plot:",
                           choices = c('submitted data', 'aggregated data'),
                           selected = "submitted data"),
              conditionalPanel("input.rp == 'submitted data'",
                 uiOutput("rankplot.measure.submitted"),
                 plotlyOutput("plot_rank_submitted")
              ),
              conditionalPanel("input.rp == 'aggregated data'",
                 uiOutput("rankplot.measure"),
                 plotlyOutput("plot_rank_aggr")
              )
)