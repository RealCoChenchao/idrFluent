#' simulator_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyWidgets
mod_simulator_module_ui <- function(id){
  ns <- NS(id)
  div(
    chooseSliderSkin("Flat", color = "#112446"),
    flowLayout(
      mod_fund_text_ui(NS(id, "box1"), fund_names[1:4]),
      mod_fund_text_ui(NS(id, "box2"), fund_names[5:8]),
      mod_fund_text_ui(NS(id, "box3"), fund_names[9:12]),
      mod_fund_text_ui(NS(id, "box4"), fund_names[13:16]),
      mod_fund_text_ui(NS(id, "box5"), fund_names[17:20]),
      Stack(
        tokens = list(childrenGap = 40),
        mod_fund_text_ui(NS(id, "box6"), c(fund_names[21:22], "ODCE")),
        PrimaryButton.shinyInput(NS(id, "calc_bt"), text = "Apply Allocation"))
      ),
    div(style = "height:20px"),
    Stack(
      tokens = list(childrenGap = 10),
      horizontal = TRUE,
      makeCard("Panel 1",
               size = 4,
               style = "max-width: 800px;",
               mod_simulator_panel_module_ui(NS(id, "panel1"),
                                             selected_val = "net_total")
               ),
      makeCard("Panel 2",
               size = 4,
               style = "max-width: 800px;",
               mod_simulator_panel_module_ui(NS(id, "panel2"),
                                             selected_val = "gross_total")
               )
      ),
    Stack(
      tokens = list(childrenGap = 10),
      horizontal = TRUE,
      makeCard("Panel 3",
               size = 4,
               style = "max-width: 800px;",
               mod_simulator_panel_module_ui(NS(id, "panel3"),
                                             selected_val = "property_type")
      ),
      makeCard("Panel 4",
               size = 4,
               style = "max-width: 800px;",
               mod_simulator_panel_module_ui(NS(id, "panel4"),
                                             selected_val = "total_leverage")
               )
      )
      )
}

#' simulator_module Server Functions
#'
#' @noRd
mod_simulator_module_server <- function(id){
  moduleServer( id, function(input, output, session){

    fund_weight <- reactive({

      fund_allocation <-
        dplyr::bind_rows(
          mod_fund_text_server("box1", fund_names[1:4])(),
          mod_fund_text_server("box2", fund_names[5:8])(),
          mod_fund_text_server("box3", fund_names[9:12])(),
          mod_fund_text_server("box4", fund_names[13:16])(),
          mod_fund_text_server("box5", fund_names[17:20])(),
          mod_fund_text_server("box6", c(fund_names[21:22], "ODCE"))()
        ) %>%
        dplyr::mutate(input_value = as.numeric(input_value),
                      rebal_value = input_value/sum(input_value, na.rm = TRUE))

      fund_allocation %>%
        dplyr::inner_join(fund_nav,
                          by = c("fund_name")) %>%
        dplyr::filter(rebal_value != 0) %>%
        dplyr::mutate(input_weight = rebal_value * total_nav) %>%
        dplyr::select(fund_name, input_weight, rebal_value)
    })

    ps_metrics <- reactive({
      intermediate_return <- calc_weight(fund_latest_return, fund_weight())
      calc_return <- wider_return_sdtr(intermediate_return,
                                       c(1, 3, 5, 7, 10, 15))

      calc_diversification <- calc_weight(fund_diversification,
                                          fund_weight(),
                                          diversification)

      calc_leverage <- calc_weight(fund_leverage, fund_weight())

      intermediate_sdtr <- calc_weight(summarized_return, fund_weight())
      calc_sdtr <- wider_return_sdtr(intermediate_sdtr,
                                     c(3, 5, 10))

      return(
        list(calc_return = calc_return,
             calc_diversification = calc_diversification,
             calc_leverage = calc_leverage,
             calc_sdtr = calc_sdtr)
        )
    }) %>% bindEvent(input$calc_bt)

    mod_simulator_panel_module_server("panel1", ps_metrics)
    mod_simulator_panel_module_server("panel2", ps_metrics)
    mod_simulator_panel_module_server("panel3", ps_metrics)
    mod_simulator_panel_module_server("panel4", ps_metrics)


  })
}

## To be copied in the UI
# mod_simulator_module_ui("simulator_module_1")

## To be copied in the server
# mod_simulator_module_server("simulator_module_1")
