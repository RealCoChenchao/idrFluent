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
#' @import writexl
#' @import stringr
#' @importFrom shinyjs useShinyjs
mod_simulator_module_ui <- function(id){
  ns <- NS(id)
  div(
    chooseSliderSkin("Flat", color = "#112446"),
    flowLayout(
      Stack(
        tokens = list(childrenGap = 40),
        mod_fund_text_ui(NS(id, "box1"), fund_names[1:4]),
        PrimaryButton.shinyInput(
          NS(id, "downloadButton_slides"),
          text = "Download Slides",
          iconProps = list(iconName = "Download")
        )),
      Stack(
        tokens = list(childrenGap = 40),
        mod_fund_text_ui(NS(id, "box2"), fund_names[5:8]),
        PrimaryButton.shinyInput(
          NS(id, "downloadButton_data"),
          text = "Download Data",
          iconProps = list(iconName = "Download")
        )),
      mod_fund_text_ui(NS(id, "box3"), fund_names[9:12]),
      mod_fund_text_ui(NS(id, "box4"), fund_names[13:16]),
      mod_fund_text_ui(NS(id, "box5"), fund_names[17:20]),
      Stack(
        tokens = list(childrenGap = 40),
        mod_fund_text_ui(NS(id, "box6"), c(fund_names[21:22], "ODCE")),
        PrimaryButton.shinyInput(NS(id, "calc_bt"), text = "Apply Allocation"))
      ),
    div(style = "height:20px"),
    shinyjs::useShinyjs(),
    div(
      style = "visibility: hidden;",
      downloadButton(NS(id, "download_slides"), label = "")
    ),
    div(
      style = "visibility: hidden;",
      downloadButton(NS(id, "download_data"), label = "")
    ),
    # div(style = "height:10px"),
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
    ns <- session$ns

    real_estate_db <- make_pool()
    # read the fund nav from the table
    fund_nav <- dplyr::tbl(real_estate_db,
                           "radar_odce_performance")  %>%
      dplyr::filter(quarter == max(quarter, na.rm = TRUE)) %>%
      dplyr::select(quarter, fund_name, total_nav = net_asset_value) %>%
      dplyr::collect()

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

      # get the ODCE and component fund leverage
      fund_leverage <- dplyr::tbl(real_estate_db, "radar_odce_performance") %>%
        dplyr::filter(quarter == max(quarter, na.rm = TRUE)) %>%
        dplyr::select(quarter, fund_name, total_leverage) %>%
        dplyr::collect()

      # read the ODCE and all component fund quarterly and historical returns
      fund_latest_return <- dplyr::tbl(real_estate_db,
                                       "radar_odce_performance") %>%
        dplyr::filter(quarter == max(quarter, na.rm = TRUE)) %>%
        dplyr::select(quarter, fund_name, contains("total_return")) %>%
        dplyr::collect()

      summarized_return <- dplyr::tbl(real_estate_db,  "ps_summarized_return") %>%
        dplyr::collect()

      # read the ODCE and all component fund sector diversification
      fund_diversification <- dplyr::tbl(real_estate_db, "radar_odce_divf") %>%
        dplyr::filter(quarter == max(quarter),
                      diverf_cat == "property_type",
                      !is.na(div_pct)) %>%
        dplyr::select(fund_name,
                      diversification,
                      total_pct = div_pct) %>%
        dplyr::collect()

      intermediate_return <- calc_weight(fund_latest_return, fund_weight())
      calc_return <- wider_return_sdtr(intermediate_return,
                                       c(1, 3, 5, 7, 10, 15))

      calc_diversification <- calc_weight(fund_diversification,
                                          fund_weight(),
                                          diversification) %>%
        mutate(diversification = stringr::str_to_title(diversification))

      calc_leverage <- calc_weight(fund_leverage, fund_weight())

      intermediate_sdtr <- calc_weight(summarized_return, fund_weight())
      calc_sdtr <- wider_return_sdtr(intermediate_sdtr,
                                     c(3, 5, 10))

      return(
        list(quarter = max(fund_latest_return$quarter, na.rm = TRUE),
             calc_return = calc_return,
             calc_diversification = calc_diversification,
             calc_leverage = calc_leverage,
             calc_sdtr = calc_sdtr)
        )
    }) %>% bindEvent(input$calc_bt)

    ps_metrics_download <- reactive({

      portfolio_composition <- fund_weight() %>%
        dplyr::select(fund_name, percentage = rebal_value) %>%
        arrange(desc(percentage))

      return <- ps_metrics()$calc_return %>%
        dplyr::select(fund_name, period = year,
                      gross_total_return = gross_total, net_total_return = net_total) %>%
        arrange(fund_name, period)

      diversification <- ps_metrics()$calc_diversification %>%
        dplyr::select(fund_name, property_type = diversification, percentage = total_pct) %>%
        arrange(fund_name, property_type)

      leverage <- ps_metrics()$calc_leverage %>%
        dplyr::select(fund_name, total_leverage) %>%
        arrange(fund_name)

      standard_deviation <- ps_metrics()$calc_sdtr %>%
        dplyr::select(fund_name, period = year, standard_deviation = total_std) %>%
        arrange(fund_name, period)

      tracking_error <- ps_metrics()$calc_sdtr %>%
        filter(fund_name != "ODCE") %>%
        dplyr::select(fund_name, period = year, tracking_error_to_odce = total_te) %>%
        arrange(fund_name, period)

      list(
        `Portfolio Composition` = portfolio_composition,
        `Total Return` = return,
        `Property Type Diversification` = diversification,
        `Portfolio Leverage` = leverage,
        `Standard Deviation` = standard_deviation,
        `Tracking Error to ODCE` = tracking_error
      )
    })

    mod_simulator_panel_module_server("panel1", ps_metrics)
    mod_simulator_panel_module_server("panel2", ps_metrics)
    mod_simulator_panel_module_server("panel3", ps_metrics)
    mod_simulator_panel_module_server("panel4", ps_metrics)

    observeEvent(input$downloadButton_slides, {
      shinyjs::click("download_slides")
    })

    output$download_slides <- downloadHandler(
      filename = function() {
        paste0("Portfolio Slides", ".pptx")
      },
      content = function(file) {
        print(generate_slides(data = ps_metrics(), fund_weight = fund_weight()), file)
      }
    )

    observeEvent(input$downloadButton_data, {
      shinyjs::click("download_data")
    })

    output$download_data <- downloadHandler(
      filename = function() {
        paste0("Portfolio Data", ".xlsx")
      },
      content = function(file) {
        writexl::write_xlsx(ps_metrics_download(), file)
      }
    )
  })
}

## To be copied in the UI
# mod_simulator_module_ui("simulator_module_1")

## To be copied in the server
# mod_simulator_module_server("simulator_module_1")
