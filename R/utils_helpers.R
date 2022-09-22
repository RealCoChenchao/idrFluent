#' helpers
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @import shiny
#' @import shiny.fluent
#' @import glue
#' @import shiny.router
#' @import tibble
#' @import dplyr
#' @import shinyWidgets
#' @importFrom plotly plotlyOutput
#' @importFrom htmltools tagList
#' @noRd
returninfoBox <- function(title, data) {
  infoBox(
    title,
    paste0(round(data*100, 2),"%"),
    icon = shiny::icon("coins"),
    color = "aqua",
    fill = TRUE
  )
}
