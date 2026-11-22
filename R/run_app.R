#' Launch the interactive Shiny application
#'
#' Opens the graphical interface of the package. It lets you load a data file
#' (tab-separated text, CSV or XLSX), choose the target, explanatory and
#' illustrative variables, fit a predictive linear discriminant analysis,
#' explore the results and predict new observations, one at a time or from
#' a file.
#'
#' @param ... Further arguments passed to [shiny::runApp()], for example
#'   `launch.browser = TRUE` or `port = 8080`.
#' @return Called for its side effect: launches the application.
#' @examples
#' if (interactive()) {
#'   run_app()
#' }
#' @export
run_app <- function(...) {

  # shiny, DT et readxl sont seulement suggérés : on vérifie leur présence
  pkgs <- c("shiny", "DT", "readxl")
  installed <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
  if (!all(installed)) {
    stop("The application requires the following package(s): ",
         paste(pkgs[!installed], collapse = ", "),
         ". Install them with install.packages().", call. = FALSE)
  }

  # Dossier de l'appli dans le package installé (copie de inst/app)
  app_dir <- system.file("app", package = "ldapred")
  if (app_dir == "") {
    stop("Could not find the application directory. ",
         "Try re-installing `ldapred`.", call. = FALSE)
  }

  shiny::runApp(app_dir, ...)
}