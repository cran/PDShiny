#' Start PDShiny
#' @title Launch PDShiny Interface
#' @return Nothing
#' @description PDShiny() loads interactive user interface built using R shiny.
#' @details The interactive user interface is to provide an easy way for people who are learning Probability Distributions. Includes example data for testing out a few example analysis.
#' @keywords PDShiny
#' @examples
#' \dontrun{
#' library(shiny)
#' PDShiny()
#' }

 PDShiny <- function() {


  shiny::runApp(appDir = system.file("shiny-examples", "myapp", package = "PDShiny"))
  Sys.setenv("R_TESTS" = "")
}
