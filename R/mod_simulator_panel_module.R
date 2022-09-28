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
mod_simulator_panel_module_ui <- function(id, selected_val = NULL){
  ns <- NS(id)
  Stack(
    tokens = list(childrenGap = 10),
    Dropdown.shinyInput(NS(id, "panel"),
                        placeHolder = "Select a metric",
                        options = ps_options,
                        value = selected_val),
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

    selectedMetric <- reactive({
      if (length(input$panel) > 0) input$panel
      else c("net_total")
    })

    output$plot <- renderPlot({
      if(selectedMetric() == "net_total"){
        # ps_dodge_plot(summary_data()$calc_return, "year", "net_total")
        ps_dodge_plot_v2(df = summary_data()$calc_return, variable = "year", value = "net_total")
      } else if(selectedMetric() == "gross_total"){
        # ps_dodge_plot(summary_data()$calc_return, "year", "gross_total")
        ps_dodge_plot_v2(df = summary_data()$calc_return, variable = "year", value = "gross_total")
      } else if(selectedMetric() == "property_type"){
        # ps_dodge_plot(summary_data()$calc_diversification, "diversification", "total_pct")
        ps_dodge_plot_v2(df = summary_data()$calc_diversification, variable = "diversification", value = "total_pct")
      } else if(selectedMetric() == "total_leverage"){
        # ps_dodge_plot(summary_data()$calc_leverage, "fund_name", "total_leverage")
        ps_dodge_plot_v2(df = summary_data()$calc_leverage, variable = "fund_name", value = "total_leverage", fill_category = FALSE)
      } else if(selectedMetric() == "total_std"){
        # ps_dodge_plot(summary_data()$calc_sdtr, "year", "total_std")
        ps_dodge_plot_v2(df = summary_data()$calc_sdtr, variable = "year", value = "total_std")
      } else if(selectedMetric() == "total_te"){
        # ps_dodge_plot(dplyr::filter(summary_data()$calc_sdtr, fund_name != "ODCE"), "year", "total_te")
        ps_dodge_plot_v2(df = dplyr::filter(summary_data()$calc_sdtr, fund_name != "ODCE"), variable = "year", value = "total_te")
      }
    })

  })
}

## To be copied in the UI
# mod_simulator_panel_module_ui("simulator_panel_module_1")

## To be copied in the server
# mod_simulator_panel_module_server("simulator_panel_module_1")
