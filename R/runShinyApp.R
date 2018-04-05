#' @title Start the shiny app for benchmarkVis package
#'
#' @description
#' Starts the shiny app from the console
#'
#' @export
runShinyApp = function() {
  app.dir = system.file("shiny", package = "benchmarkVis")
  if (app.dir == "") {
    stop("Could not find example directory. Try re-installing `benchmarkVis`.", call. = FALSE)
  }
  shiny::runApp(app.dir, display.mode = "normal")
}
