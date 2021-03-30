# Title     : gwpcorMapper
# Objective : Wrapper to launch gwpcorMapper.
# Created by: Joseph Percival
# Created on: 2021/02/26
#' Launches gwpcorMapper.
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @return null
#' @examples
#' \dontrun{
#' gwpcormapper::run_app()
#' }
run_app <- function(
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(...)
  )
}
