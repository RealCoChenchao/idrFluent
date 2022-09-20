#' filter_module UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny.fluent
#' @import plotly
#' @import ggplot2
#' @import highcharter
#' @import shinyWidgets
#' @import shiny

mod_filter_module_ui <- function(id){
  ns <- NS(id)
  div(
    chooseSliderSkin("Flat", color = "#112446"),
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      makeCard("Filters",
             Stack(
               tokens = list(childrenGap = 10),
               Stack(
                 horizontal = TRUE,
                 tokens = list(childrenGap = 10),
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
                                     options = fund_options),
                 Dropdown.shinyInput(NS(id, "sector"),
                                     placeHolder = "Property type",
                                     multiSelect = TRUE,
                                     styles = list(
                                       dropdownItemsWrapper = list(
                                         maxHeight = "200px",
                                         overflow = "auto"
                                       )),
                                     options = sector_options),
                 # Dropdown.shinyInput(NS(id, "state"),
                 #                     placeHolder = "State",
                 #                     options = metro_options),
                 Dropdown.shinyInput(NS(id, "metro"),
                                     placeHolder = "Metro",
                                     multiSelect = TRUE,
                                     dropdownWidth = 'auto',
                                     styles = list(
                                       dropdownItemsWrapper = list(
                                         root = list(width = "10vw"),
                                         maxHeight = "200px",
                                         overflow = "auto"
                                       )),
                                     options = metro_options)
               ),
               Slider.shinyInput(NS(id, "year"),
                                 value = 2010, min = 2000, max = 2022, step = 2,
                                 label = "Year Built",
                                 snapToStep = TRUE
               ),
               Slider.shinyInput(NS(id, "sqft"),
                                 value = 10000, min = 5000, max = 1e6, step = 5000,
                                 label = "Sqft",
                                 snapToStep = TRUE
               ),
               Toggle.shinyInput(NS(id, "operatingOnly"),
                                 value = TRUE,
                                 label = "Include operating properties only?"),
               Toggle.shinyInput(NS(id, "clusterOnly"),
                                 value = TRUE,
                                 label = "Show property in clusters?")
             ),
             size = 3,
             style = "max-height: 500px;"),
      makeCard("Sector count",
               highchartOutput(NS(id, "plot"),
                            width = 1000),
               size = 8,
               style = "max-height: 500px")
  ),
  Stack(
    tokens = list(childrenGap = 10),
    makeCard("Top results",
             size = 5,
             div(style="max-height: 320px; overflow: auto",
                 uiOutput(NS(id, "query_table")))),
    makeCard("Map",
             size = 5,
             div(style="max-height: 800px; overflow: auto",
                 leafletOutput(NS(id, "query_map"),
                               height = 800,
                               width = 1500)
           ))
    )
  )
}

#' filter_module Server Functions
#'
#' @noRd
mod_filter_module_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    filtered_property <- reactive({

      # if(input$quarter == "2021 Q1"){
      #   tidy_sf <- full_tidy_sf_2021q1
      # }else if(input$quarter == "2021 Q2"){
      #   tidy_sf <- full_tidy_sf_2021q2
      # }else{
      #   tidy_sf <- full_tidy_sf_2021q3
      # }

      selectedYear <- (
        if (length(input$year) > 0) input$year
        else c(2010)
      )

      selectedSqft <- (
        if (length(input$sqft) > 0) input$sqft
        else c(10000)
      )

      selectedFund <- (
        if (length(input$fund) > 0) input$fund
        else unique(tidy_sf$fund_name)
      )

      selectedSector <- (
        if (length(input$sector) > 0) input$sector
        else unique(tidy_sf$property_type)
      )

      selectedMetro <- (
        if (length(input$metro) > 0) input$metro
        else unique(tidy_sf$GEOID)
      )

      minOperatingVal <- if (isTRUE(input$operatingOnly)) 1 else 0

      result <- tidy_sf %>%
        dplyr::mutate(
          operating = ifelse(property_life_cycle == "Operating",
                             1,
                             0)) %>%
        dplyr::filter(
          GEOID %in% selectedMetro,
          year_built >= selectedYear,
          square_feet >= selectedSqft,
          operating >= minOperatingVal,
          fund_name %in% selectedFund,
          property_type %in% selectedSector
        )

      result
    })

    filtered_layer <- reactive({

      selectedSector <- (
        if (length(input$sector) > 0) input$sector
        else unique(tidy_sf$property_type)
      )

      base_sector <- c("Office",
                       "Industrial",
                       "Apartment",
                       "Retail")

      ifelse(selectedSector %in% base_sector,
             selectedSector,
             "Other")
    })

    output$query_map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("Stamen.Watercolor") %>%
        addProviderTiles("Stamen.TonerHybrid") %>%
        addPortfolioMarkers(df = filtered_property(),
                            selected_sector = "Office",
                            cluster_option = input$clusterOnly) %>%
        addPortfolioMarkers(df = filtered_property(),
                            selected_sector = "Industrial",
                            cluster_option = input$clusterOnly) %>%
        addPortfolioMarkers(df = filtered_property(),
                            selected_sector = "Retail",
                            cluster_option = input$clusterOnly) %>%
        addPortfolioMarkers(df = filtered_property(),
                            selected_sector = "Apartment",
                            cluster_option = input$clusterOnly) %>%
        addPortfolioMarkers(df = filtered_property(),
                            selected_sector = "Other",
                            cluster_option = input$clusterOnly) %>%
        addLayersControl(baseGroups = c("Contrast Map",
                                        "Street Map",
                                        "Satellite Map",
                                        "Transit Map"),
                         overlayGroups = filtered_layer(),
                         position = "topright",
                         options = layersControlOptions(collapsed = FALSE))
    })

    output$query_table <- renderUI({
      items_list <- DetailsList(items = filtered_property() %>%
                             sf::st_drop_geometry(),
                           columns = details_list_columns)
      items_list
    })

    output$plot <- renderHighchart({
      ct_df <- dplyr::count(sf::st_drop_geometry(filtered_property()),
                   fund_name,
                   property_type,
                   .drop = TRUE) %>%
        dplyr::rename(Fund = fund_name,
                      Sector = property_type,
                      Count = n)

      hchart(ct_df, "column",
             hcaes(x = Fund,
                   y = Count,
                   group = Sector))
    })

  })
}

## To be copied in the UI
# mod_filter_module_ui("filter_module_1")

## To be copied in the server
# mod_filter_module_server("filter_module_1")
