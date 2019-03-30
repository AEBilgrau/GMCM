#' Run the GMCM shiny application
#'
#' Function for starting a local instance of the GMCM shiny application.
#' The online application is found at \link{https://gmcm.shinyapps.ui/GMCM/}.
#'
#' @param ... Arguments passed to \code{\link[shiny]{runApp}}.
#' @return Retuns nothing (usually). See \code{\link[shiny]{runApp}}.
#'   Exit or stop the app by interrupting R.
#' @seealso \code{\link[shiny]{runApp}}
#' @examples
#' \dontrun{
#' runGMCM()
#' runGMCM(launch.browser = FALSE, port = 1111)
#' # Open browser and enter URL http://127.0.0.1:1111/
#' }
#' @export
runGMCM <- function(...) { # nocov start
  # Ensure suggested shiny package
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("The 'shiny' package is needed for this function to work.",
         call. = FALSE)
  }
  # Run app
  shiny::runApp(appDir = system.file("shiny", package = "GMCM"),
                ...)
} # nocov end
