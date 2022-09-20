#' fund_text UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fund_text_ui <- function(id, name_fund){
  ns <- NS(id)
  Stack(
    tokens = list(childrenGap = 10),
    purrr::map(name_fund,
               ~TextField.shinyInput(NS(id, .x), label = .x))
  )

}

#' fund_text Server Functions
#'
#' @noRd
mod_fund_text_server <- function(id, name_fund){
  moduleServer( id, function(input, output, session){

    output$alloct <- reactive({
      tibble(input_value = purrr::map(name_fund, ~input[[.x]]),
             fund_name = name_fund)
    })

  })
}

## To be copied in the UI
# mod_fund_text_ui("fund_text_1")

## To be copied in the server
# mod_fund_text_server("fund_text_1")
