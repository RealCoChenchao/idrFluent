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
      )
      ),


      Stack(
        tokens = list(childrenGap = 10),
        horizontal = TRUE,
        makeCard("Panel 1",
                 size = 4,
                 style = "max-width: 800px;",
                 Stack(
                   tokens = list(childrenGap = 10),
                   Dropdown.shinyInput(NS(id, "panel1"),
                                       placeHolder = "Select a metric",
                                       options = fund_options),
                   uiOutput(NS(id, "p1_table"))
                   )
                 ),
        makeCard("Panel 2",
                 size = 4,
                 style = "max-width: 800px;",
                 Stack(
                   tokens = list(childrenGap = 10),
                   Dropdown.shinyInput(NS(id, "panel2"),
                                       placeHolder = "Select a metric",
                                       options = fund_options),
                   uiOutput(NS(id, "p2_table"))
                   )
                 )
        ),
      Stack(
        tokens = list(childrenGap = 10),
        horizontal = TRUE,
        makeCard("Panel 3",
                 size = 4,
                 style = "max-width: 800px;",
                 Stack(
                   tokens = list(childrenGap = 10),
                   Dropdown.shinyInput(NS(id, "panel3"),
                                       placeHolder = "Select a metric",
                                       options = fund_options),
                   uiOutput(NS(id, "p3_table"))
                 )
        ),
        makeCard("Panel 4",
                 size = 4,
                 style = "max-width: 800px;",
                 Stack(
                   tokens = list(childrenGap = 10),
                   Dropdown.shinyInput(NS(id, "panel4"),
                                       placeHolder = "Select a metric",
                                       options = fund_options),
                   uiOutput(NS(id, "p4_table"))
                   )
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

    output$p1_table <- renderUI({
      items_list <- DetailsList(items = head(tidy_sf) %>%
                                  sf::st_drop_geometry(),
                                columns = details_list_columns)
      items_list
    })

    output$p2_table <- renderUI({
      items_list <- DetailsList(items = head(tidy_sf) %>%
                                  sf::st_drop_geometry(),
                                columns = details_list_columns)
      items_list
    })

    output$p3_table <- renderUI({
      items_list <- DetailsList(items = head(tidy_sf) %>%
                                  sf::st_drop_geometry(),
                                columns = details_list_columns)
      items_list
    })

    output$p4_table <- renderUI({
      items_list <- DetailsList(items = head(tidy_sf) %>%
                                  sf::st_drop_geometry(),
                                columns = details_list_columns)
      items_list
    })

    port_weight = reactiveValues(weight=rep(1/12, 12))

    # If any of the sliders change, then recalculate other weight weights to satisfy sum to 1 constraint
    observers <- purrr::map(seq(1, 12, 1),
                            .f = ~{
                              input_name <- glue("f{.x}")
                              observeEvent(input[[input_name]],
                                           {
                                             suspendMany(observers) #This function comes from shinyhelper.R
                                             port_weight$weight = updateweight(port_weight$weight, input[[input_name]], .x)
                                             resumeMany(observers) #This function comes from shinyhelper.R
                                           }
                              )
                            })


      # observeEvent(input$f1,
      #              {
      #                suspendMany(observers) #This function comes from shinyhelper.R
      #                port_weight$weight = updateweight(port_weight$weight, input$f1, 1)
      #                resumeMany(observers) #This function comes from shinyhelper.R
      #              }
      # ),
      # observeEvent(input$f2,
      #              {
      #                suspendMany(observers)
      #                port_weight$weight = updateweight(port_weight$weight, input$f2, 2)
      #                resumeMany(observers)
      #              }
      # ),
      # observeEvent(input$f3,
      #              {
      #                suspendMany(observers)
      #                port_weight$weight = updateweight(port_weight$weight, input$f3, 3)
      #                resumeMany(observers)
      #              }
      # )

    # If the weights change, update the sliders
    # output$f1ui = renderUI({
    #   wghtsliderInput(NS(id, "f1"), port_weight$weight[1], label = "AEW") #This function comes from shinyhelper.R
    # })
    # output$f2ui = renderUI({
    #   wghtsliderInput(NS(id, "f2"), port_weight$weight[2], label = "Ara")
    # })
    # output$f3ui = renderUI({
    #   wghtsliderInput(NS(id, "f3"), port_weight$weight[3], label = "ASB")
    # })

    purrr::walk2(seq(1, 12, 1),
                 purrr::map(fund_options, ~.x$key)[1:12],
                 .f = ~{
                   output_name <- glue("f{.x}ui")
                   output[[output_name]] <- renderUI({
                     wghtsliderInput(NS(id, glue("f{.x}")),
                                     port_weight$weight[.x],
                                     label = .y
                                     )
                     })
                   })
  })
}

## To be copied in the UI
# mod_simulator_module_ui("simulator_module_1")

## To be copied in the server
# mod_simulator_module_server("simulator_module_1")
