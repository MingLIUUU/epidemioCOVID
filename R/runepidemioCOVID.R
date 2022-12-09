#' Run shiny App for epidmioCOVID
#'
#'
#' A function that launches the Shiny app for epidemioCOVID
#' The purpose of this app is only to illustrate how a Shiny
#' app works. The code has been placed in \code{./inst/shiny-scripts}.
#'
#' @return No return value but open up a Shiny page.
#'
#' @examples
#' \dontrun{
#'
#' epidemioCOVID::runepidemioCOVID()
#' }
#'
#' @references
#' Grolemund, G. (2015). Learn Shiny - Video Tutorials.
#' \href{https://shiny.rstudio.com/tutorial/}{Link}
#'
#' @export
#' @importFrom shiny runApp
#'
runepidemioCOVID <- function() {
  appDir <- system.file("shiny-scripts",
                        package = "epidemioCOVID")
  shiny::runApp(appDir, display.mode = "normal")
  return()
}
# [END]
