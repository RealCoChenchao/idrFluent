#' raster_view UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom leaflet colorNumeric
#' @import rcAppTools
#' @importFrom leaflet addLayersControl
mod_raster_view_ui <- function(id){
  ns <- NS(id)
  div(
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      makeCard("Filters",
                 Stack(
                   tokens = list(childrenGap = 10),
                   Dropdown.shinyInput(NS(id, "sector"),
                                       placeHolder = "Property type",
                                       value = "multifamily",
                                       styles = list(
                                         dropdownItemsWrapper = list(
                                           maxHeight = "200px",
                                           overflow = "auto"
                                         )),
                                       options = raster_sector_options),
                   Dropdown.shinyInput(NS(id, "metro"),
                                       placeHolder = "Metro",
                                       value = "408",
                                       dropdownWidth = 'auto',
                                       styles = list(
                                         dropdownItemsWrapper = list(
                                           root = list(width = "10vw"),
                                           maxHeight = "200px",
                                           overflow = "auto"
                                         )),
                                       options = metro_options),
                   Dropdown.shinyInput(NS(id, "metric"),
                                       placeHolder = "Total Population",
                                       value = "total_population",
                                       options = raster_metric_options),
                   Dropdown.shinyInput(NS(id, "radius"),
                                       value = "___5___minutes",
                                       placeHolder = "5min drive",
                                       options = raster_radius_options),
                   Toggle.shinyInput(NS(id, "percentile"),
                                     value = FALSE,
                                     label = "Apply percentile?"),
                   Toggle.shinyInput(NS(id, "fund"),
                                     value = FALSE,
                                     label = "Filter by fund (Disenabled)"),
                   conditionalPanel(
                     sprintf("input['%s'] != ''", ns("fund")),

                     Stack(
                       tokens = list(childrenGap = 10),
                       Dropdown.shinyInput(NS(id, "fund_name"),
                                           placeHolder = "Component fund",
                                           multiSelect = TRUE,
                                           styles = list(
                                             dropdownItemsWrapper = list(
                                               maxHeight = "200px",
                                               overflow = "auto"
                                             )),
                                           options = fund_options))
                   ),
                   PrimaryButton.shinyInput(NS(id, "render"), text = "Render Map")
                 ),
               size = 3,
               style = "max-height: 800px;"),
      makeCard("Plot placeholder",
               highchartOutput(NS(id, "plot"),
                               width = 1200),
               size = 8,
               style = "max-height: 800px")
    ),
    Stack(
      tokens = list(childrenGap = 10),
      makeCard("Map",
               size = 5,
               div(style="max-height: 800px; overflow: auto",
                   leafletOutput(NS(id, "map"),
                                 height = 800,
                                 width = 1500)
               ))
    )
  )
}

#' raster_view Server Functions
#'
#' @noRd
mod_raster_view_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    annual_raster_manifest <- summary_raster_manifest("annual_demographic_raster/property_type")
    monthly_raster_manifest <- summary_raster_manifest("monthly_allsector_raster/property_type")

    googlesheets4::gs4_auth(cache = ".secrets",
                            email = TRUE)
    raster_variables <- googlesheets4::read_sheet("1CRwiTtRTyl_8zFSyrsn2oXizGTYY0QSNtyJ1H5nRQPg") %>%
      dplyr::filter(idr_app == "Y")

    observe({
      update_options <- raster_variables %>%
        dplyr::filter(property_type == input$sector) %>%
        dplyr::select(text = english_name,
                      key = variable_name) %>%
        jsonlite::toJSON(dataframe = "rows")

      updateDropdown.shinyInput(inputId = "metric",
                                options = update_options)
      }) %>%
      bindEvent(input$sector)

    selected_raster_list <- reactive({
      selectedMetro <- (
        if (length(input$metro) > 0) input$metro
        else c("408")
      )

      selectedSector <- (
        if (length(input$sector) > 0) input$sector
        else c("multifamily")
      )

      annual_raster_list <- annual_raster_manifest %>%
        dplyr::filter(GEOID == selectedMetro,
                      property_type == selectedSector) %>%
        aws.s3::s3readRDS(object = .$Key,
                          bucket = .$Bucket)

      monthly_raster_list <- monthly_raster_manifest %>%
        dplyr::filter(GEOID == selectedMetro,
                      property_type == selectedSector) %>%
        aws.s3::s3readRDS(object = .$Key,
                          bucket = .$Bucket)

      raster_list <- c(annual_raster_list, monthly_raster_list)
    })

    selected_raster <- reactive({
      selectedMetric <- (
        if (length(input$metric) > 0) input$metric
        else c("total_population")
      )

      selectedRadius <- (
        if (length(input$radius) > 0) input$radius
        else c("___5___minutes")
      )

      retrieve_raster_info(selected_raster_list(),
                           raster_variables,
                           selectedMetric,
                           selectedRadius)
    }) %>%
      bindCache(input$metro, input$sector, input$metric, input$radius) %>%
      bindEvent(input$render)

    output$map <- renderLeaflet({
      req(selected_raster())
      selectedPercentile <- (
        if (isTRUE(input$percentile)) c("Percentile")
        else c("Continuous")
      )

      rcAppTools::plot_raster(selected_raster(), selectedPercentile)
    })

  })
}

## To be copied in the UI
# mod_raster_view_ui("raster_view_1")

## To be copied in the server
# mod_raster_view_server("raster_view_1")
