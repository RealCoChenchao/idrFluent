#' simulator_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_simulator_module_ui <- function(id){
  ns <- NS(id)
  div(
    flowLayout(
      Stack(
        tokens = list(childrenGap = 2),
        uiOutput(NS(id, "f1ui")),
        uiOutput(NS(id, "f2ui")),
        uiOutput(NS(id, "f3ui")),
        uiOutput(NS(id, "f4ui")),
      ),
      Stack(
        tokens = list(childrenGap = 2),
        uiOutput(NS(id, "f5ui")),
        uiOutput(NS(id, "f6ui")),
        uiOutput(NS(id, "f7ui")),
        uiOutput(NS(id, "f8ui")),
      ),
      Stack(
        tokens = list(childrenGap = 2),
        uiOutput(NS(id, "f9ui")),
        uiOutput(NS(id, "f10ui")),
        uiOutput(NS(id, "f11ui")),
        uiOutput(NS(id, "f12ui")),
      ),
      Stack(
        tokens = list(childrenGap = 2),
        uiOutput(NS(id, "f13ui")),
        uiOutput(NS(id, "f14ui")),
        uiOutput(NS(id, "f15ui")),
        uiOutput(NS(id, "f16ui")),
      ),
      Stack(
        tokens = list(childrenGap = 2),
        uiOutput(NS(id, "f17ui")),
        uiOutput(NS(id, "f18ui")),
        uiOutput(NS(id, "f19ui")),
        uiOutput(NS(id, "f20ui")),
      ),
      Stack(
        tokens = list(childrenGap = 2),
        uiOutput(NS(id, "f21ui")),
        uiOutput(NS(id, "f22ui"))
      )
      ),


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

    port_weight <- reactiveValues(weight=rep(100/22, 22))
    # slider_debounced <- reactiveValues()
    # slider_debounced <- purrr::map(seq(1, 22, 1),
    #                                .f = ~{
    #                                  input_name <- glue("f{.x}")
    #                                  slider_debounced[[input_name]] <- reactive(input[[input_name]]) %>%
    #                                    debounce(200)
    #                                })

    # If any of the sliders change, then recalculate other weight weights to satisfy sum to 1 constraint
    observers <- purrr::map(seq(1, 22, 1),
                            .f = ~{
                              input_name <- glue("f{.x}")
                              observeEvent(input[[input_name]],
                                           {
                                             suspendMany(observers)
                                             port_weight$weight = updateweight(port_weight$weight,
                                                                               input[[input_name]],
                                                                               .x)
                                             resumeMany(observers)
                                           }
                              )
                            })

    purrr::walk2(seq(1, 22, 1),
                 purrr::map(fund_options, ~.x$key)[1:22],
                 .f = ~{
                   output_name <- glue("f{.x}ui")
                   output[[output_name]] <- renderUI({
                     wghtsliderInput(NS(id, glue("f{.x}")),
                                     port_weight$weight[.x],
                                     label = .y
                                     )
                     })
                   })

    fund_weight <- reactive({

      fund_allocation <-
        tibble(input_value = port_weight$weight,
               fund_name = unlist(purrr::map(fund_options, ~.x$key)[1:22]))

      nav_weight <- fund_allocation %>%
        dplyr::inner_join(fund_nav,
                          by = c("fund_name")) %>%
        dplyr::filter(input_value != 0) %>%
        dplyr::mutate(input_weight = input_value * total_nav) %>%
        dplyr::select(fund_name, input_weight,
                      total_nav, input_value)

      return(nav_weight)
    })

    ps_metrics <- reactive({
      intermediate_return <- calc_weight(fund_latest_return, fund_weight())
      calc_return <- wider_return_sdtr(intermediate_return,
                                       c(1, 3, 5, 7, 10, 15))

      calc_diversification <- calc_weight(fund_diversification,
                                          fund_weight(),
                                          property_type)

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
    })

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
