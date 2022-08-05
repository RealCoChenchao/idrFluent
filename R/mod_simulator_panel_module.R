#' simulator_panel_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom stringr str_extract
#' @importFrom forcats fct_reorder
mod_simulator_panel_module_ui <- function(id){
  ns <- NS(id)
  Stack(
    tokens = list(childrenGap = 10),
    Dropdown.shinyInput(NS(id, "panel"),
                        placeHolder = "Select a metric",
                        options = fund_options),
    makesimpleCard(plotOutput(NS(id, "plot"),
                              width = 600),
                   size = 8,
                   style = "max-height: 800px")
  )
}

#' simulator_panel_module Server Functions
#'
#' @noRd
mod_simulator_panel_module_server <- function(id, summary_data){
  moduleServer(id, function(input, output, session){

    output$plot <- renderPlot({
      summary_data() %>%
        dodge_plot(x_value = vintage_group,
                   order_by = vintage)
    })

  })
}

## To be copied in the UI
# mod_simulator_panel_module_ui("simulator_panel_module_1")

## To be copied in the server
# mod_simulator_panel_module_server("simulator_panel_module_1")
