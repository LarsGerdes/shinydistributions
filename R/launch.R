#' Launch the shiny app shinydistributions.
#'
#' @export launch
#'
#' @return A shiny application object.
#'
# @example \dontrun {launch()}

# run the app
#launch <- function() {
#  #shinydistributions <-
#  shiny::shinyApp(ui = ui, server = server)
#  #shiny::runApp(shinydistributions)
#}


launch <- function() {
  shiny::runApp(appDir = system.file("app", package = "shinydistributions"))
}
