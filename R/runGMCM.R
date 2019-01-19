#' Run the GMCM shiny app
#'
#' Function for locally running the GMCM shiny application.
#'
#' @param ... Arguments passed to \code{\link[shiny]{runApp}}.
#' @return Retuns nothing (usually). See \code{\link[shiny]{runApp}}.
#'   Exit or stop the app by interrupting R.
#' @seealso \code{\link[shiny]{runApp}}
#' @examples
#' \dontrun{
#' runGMCM()
#' runGMCM(launch.browser = FALSE, port = 1111)
#' }
#' @export
runGMCM <- function(...) {
  # Ensure suggested shiny package
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is needed for this function to work.",
         call. = FALSE)
  }
  # Run app
  shiny::runApp(appDir = system.file("shiny", package = "GMCM"),
                ...)
}
