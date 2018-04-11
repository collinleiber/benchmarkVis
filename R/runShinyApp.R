#' @title Start the shiny app for benchmarkVis package
#'
#' @description
#' Starts the shiny app from the console
#'
#' @export
runShinyApp = function() {
  packages.required = c("shiny", "shinydashboard", "shinyjs", "V8",	"shinyBS", "DT")
  for (package in packages.required) {
    if (!requireNamespace(package, quietly = TRUE)) {
      stop(paste0("Package \"", package, "\" needed for shiny app to work. Please install it."),
        call. = FALSE)
    }
  }
  app.dir = system.file("shiny", package = "benchmarkVis")
  if (app.dir == "") {
    stop("Could not find example directory. Try re-installing `benchmarkVis`.", call. = FALSE)
  }
  shiny::runApp(app.dir, display.mode = "normal")
}
