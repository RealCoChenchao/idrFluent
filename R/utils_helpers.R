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


## Funcions to balance weight
#Updates weights if changed
updateweight = function(oldweight, new, i) {
  if (new==oldweight[i]) {
    oldweight
  } else if (new==100){
    newweight = rep(0,22)
    oldweight = oldweight
    new = 99.9999
    newweight[-i] = oldweight[-i]/(sum(oldweight[-i]) + 1e-10)*(100-new)
    newweight[i] = new
    newweight
  } else {
    newweight = rep(0,22)
    oldweight = oldweight
    newweight[-i] = oldweight[-i]/(sum(oldweight[-i]) + 1e-10)*(100-new)
    newweight[i] = new
    newweight
  }
}

# suspend and resume a list of observers
suspendMany = function(observers) invisible(lapply(observers, function(x) x$suspend()))
resumeMany = function(observers) invisible(lapply(observers, function(x) x$resume()))

# function to change sliderInput
wghtsliderInput <- function(inputId, value, label, submitted=FALSE) {
  if (!submitted)
    # Slider.shinyInput(inputId,
    #                   value=value,
    #                   label=label,
    #                   valueFormat = JS("function(x) { return Math.round(x * 100) / 100 + '%'}"),
    #                   # snapToStep = TRUE,
    #                   # step = 5,
    #                   min=0,
    #                   max=100)
    sliderInput(inputId,
               value=value,
               label=label,
               min=0,
               max=100,
               ticks = FALSE)

}

returninfoBox <- function(title, data) {
  infoBox(
    title,
    paste0(data*100,"%"),
    icon = shiny::icon("coins"),
    color = "aqua",
    fill = TRUE
  )
}
