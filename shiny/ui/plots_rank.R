tabpanel.plots_rank =  list(
            h3("Rank plot for your data"),             
              conditionalPanel("input.datatype == 'submitted data'",
                 uiOutput("rankplot.measure.submitted"),
                 plotlyOutput("plot_rank_submitted")
              ),
              conditionalPanel("input.datatype == 'aggregated data'",
                 uiOutput("rankplot.measure"),
                 plotlyOutput("plot_rank_aggr")
              )
)