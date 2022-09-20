#' fund_exp_filter UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @import shinydashboard
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
                            multiSelect = TRUE,
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
                          options = quarter_options)
    ),
    Stack(horizontal = TRUE,
          tokens = list(childrenGap = 10),
          Stack(tokens = list(childrenGap = 10),
              makeCard("Sector count",
                     highchartOutput(NS(id, "plot"),
                                     width = 500),
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

    preped_info_return <- reactive({

      selectedFund <- (
        if (length(input$fund) > 0) input$fund
        else c("AEW")
      )

      selectedQuarter <- (
        if (length(input$fund) > 0) input$fund
        else c("2022 Q1")
      )

      tbl(real_estate_db,
          "radar_odce_total_return") %>%
        dplyr::select(quarter,
                      fund_name,
                      net_total_return,
                      net_income_return,
                      net_appreciation_return) %>%
        dplyr::collect() %>%
        dplyr::filter(quarter == selectedQuarter,
                      fund_name == selectedFund)
    })

    output$a_return <- renderInfoBox({
      returninfoBox("A return",
                    preped_info_return()$net_appreciation_return)
    })

    output$i_return <- renderInfoBox({
      returninfoBox("I return",
                    preped_info_return()$net_income_return)
    })

    output$t_return <- renderInfoBox({
      returninfoBox("T return",
                    preped_info_return()$net_total_return)
    })

    output$property <- renderInfoBox({
      infoBox(
        "Property",
        paste0(25, "%"),
        icon = shiny::icon("building"),
        color = "aqua",
        fill = TRUE
      )
    })

    output$investment <- renderInfoBox({
      infoBox(
        "Investment",
        paste0(25, "%"),
        icon = shiny::icon("sack-dollar"),
        color = "aqua",
        fill = TRUE
      )
    })

  })
}

## To be copied in the UI
# mod_fund_exp_filter_ui("fund_exp_filter_1")

## To be copied in the server
# mod_fund_exp_filter_server("fund_exp_filter_1")
