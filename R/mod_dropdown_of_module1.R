#' dropdown_of_module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny.fluent
mod_dropdown_of_module1_ui <- function(id, placeholder, options){
  ns <- NS(id)
  Dropdown.shinyInput(ns("select"),
                      placeHolder = placeholder,
                      options = options)
}

#' dropdown_of_module1 Server Functions
#'
#' @noRd
mod_dropdown_of_module1_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    result <- reactive({
      input$select
    })
    return(result)
  })
}

## To be copied in the UI
# mod_dropdown_of_module1_ui("dropdown_of_module1_1")

## To be copied in the server
# mod_dropdown_of_module1_server("dropdown_of_module1_1")
