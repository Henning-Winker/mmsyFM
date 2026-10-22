#' Launch the mmsyFM Shiny app
#'
#' Opens the app with six tabs: set-up (system, status quo, tuning),
#' equilibrium explorer, yield curves, safeguarded MMSY, harvest strategies
#' and trigger tuning.
#'
#' @param ... passed to [shiny::runApp()].
#' @return Called for its side effect.
#' @export
run_app <- function(...) {
  if (!requireNamespace("shiny", quietly = TRUE))
    stop("The app needs the shiny package: install.packages(\"shiny\")", call. = FALSE)
  shiny::runApp(system.file("shiny", "mmsyFM", package = "mmsyFM"), ...)
}
