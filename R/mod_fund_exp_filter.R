#' fund_exp_filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shinydashboard
#' @import dplyr
#' @importFrom shiny NS tagList
#' @export
mod_fund_exp_filter_ui <- function(id){
  ns <- NS(id)
  div(
      Stack(
        horizontal = TRUE,
        tokens = list(childrenGap = 200),
        Dropdown.shinyInput(NS(id, "quarter"),
                            placeHolder = "Quarter",
                            options = quarter_options),
        Dropdown.shinyInput(NS(id, "fund"),
                            placeHolder = "Component fund",
                            multiSelect = FALSE,
                            styles = list(
                              dropdownItemsWrapper = list(
                                maxHeight = "200px",
                                overflow = "auto"
                              )),
                            options = fund_options)
    ),
    br(),
    Stack(horizontal = TRUE,
          tokens = list(childrenGap = 10),
          makesimpleCard(infoBoxOutput(NS(id,"a_return"))),
          makesimpleCard(infoBoxOutput(NS(id,"i_return"))),
          makesimpleCard(infoBoxOutput(NS(id,"t_return"))),
          makesimpleCard(infoBoxOutput(NS(id,"property"))),
          makesimpleCard(infoBoxOutput(NS(id,"investment")))
          ),
    Stack(horizontal = TRUE,
          tokens = list(childrenGap = 10),
          Dropdown.shinyInput(NS(id, "diversification"),
                              placeHolder = "Diversification",
                              options = diverf_options)
    ),
    br(),
    Stack(horizontal = TRUE,
          tokens = list(childrenGap = 10),
          Stack(tokens = list(childrenGap = 10),
              makeCard("Diversification",
                       plotOutput(NS(id, "plot"),
                                     width = 800),
                       size = 8,
                       style = "max-height: 500px"),
              makeCard("Top results",
                       size = 5,
                       div(style="max-height: 320px; overflow: auto",
                           uiOutput(NS(id, "query_table")))
              )),
          makeCard("Map",
                   size = 5,
                   div(style="max-height: 800px; overflow: auto",
                       leafletOutput(NS(id, "query_map"),
                                     height = 800,
                                     width = 550)
                   ))
      )
  )

}

#' fund_exp_filter Server Functions
#'
#' @noRd
mod_fund_exp_filter_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # for the infoboxes
    fund_exp <- reactive({
      real_estate_db <- make_pool()
      selectedFund <- (
        if (length(input$fund) > 0) input$fund
        else c("AEW")
      )

      selectedQuarter <- (
        if (length(input$quarter) > 0) input$quarter
        else c("2022 Q1")
      )

      filtered_pefm <- tbl(real_estate_db,
                           "radar_odce_performance") %>%
        dplyr::select(quarter,
                      fund_name,
                      net_total_return,
                      net_income_return,
                      net_appreciation_return) %>%
        dplyr::filter(quarter == !!selectedQuarter,
                      fund_name == !!selectedFund) %>%
        dplyr::collect()

      filtered_invest <- tbl(real_estate_db,
                            "cf_fund_level") %>%
        dplyr::filter(fund_level_data == "Number of Investments") %>%
        dplyr::filter(quarter == !!selectedQuarter,
                      fund_name == !!selectedFund) %>%
        dplyr::collect()

      filtered_property <- tbl(real_estate_db,
                             "cf_fund_level") %>%
        dplyr::filter(fund_level_data == "Number of Properties") %>%
        dplyr::filter(quarter == !!selectedQuarter,
                      fund_name == !!selectedFund) %>%
        dplyr::collect()

      return(list(total_return = filtered_pefm$net_total_return,
                  income_return = filtered_pefm$net_income_return,
                  appreciation_return = filtered_pefm$net_appreciation_return,
                  count_invest = filtered_invest$value,
                  count_property = filtered_property$value))
    })

    output$a_return <- renderInfoBox({
      returninfoBox("A return",
                    fund_exp()$appreciation_return)
    })

    output$i_return <- renderInfoBox({
      returninfoBox("I return",
                    fund_exp()$income_return)
    })

    output$t_return <- renderInfoBox({
      returninfoBox("T return",
                    fund_exp()$total_return)
    })

    output$property <- renderInfoBox({
      infoBox(
        "Property",
        paste0(fund_exp()$count_property),
        icon = shiny::icon("building"),
        color = "aqua",
        fill = TRUE
      )
    })

    output$investment <- renderInfoBox({
      infoBox(
        "Investment",
        paste0(fund_exp()$count_invest),
        icon = shiny::icon("sack-dollar"),
        color = "aqua",
        fill = TRUE
      )
    })

    # for diversification
    fund_diverf <- reactive({

      selectedFund <- (
        if (length(input$fund) > 0) input$fund
        else c("AEW")
      )

      selectedQuarter <- (
        if (length(input$quarter) > 0) input$quarter
        else c("2022 Q1")
      )

      selectedDiverf <- (
        if (length(input$diversification) > 0) input$diversification
        else c("property_type")
      )

      real_estate_db <- make_pool()
      filtered_divf <- tbl(real_estate_db,
                     "radar_compfund_divf") %>%
        dplyr::filter(!is.na(div_pct),
                      quarter == !!selectedQuarter,
                      fund_name == !!selectedFund,
                      diverf_cat == !!selectedDiverf) %>%
        dplyr::mutate(diversification = stringr::str_to_title(diversification)) %>%
        dplyr::collect()

      return(filtered_divf)
    })

    output$plot <- renderPlot({
      fund_exp_diverf_plot(fund_diverf())
    })
  })
}

## To be copied in the UI
# mod_fund_exp_filter_ui("fund_exp_filter_1")

## To be copied in the server
# mod_fund_exp_filter_server("fund_exp_filter_1")
