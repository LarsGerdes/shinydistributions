#' Launch the shiny app shinydistributions.
#'
#' @export launch
#'
#' @return A shiny application object.
#'
#' @example \dontrun {launch()}

launch <- function() {
  shiny::runApp(appDir = system.file("app", package = "shinydistributions"))
}
