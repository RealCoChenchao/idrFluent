#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import leaflet
#' @noRd
app_server <- function(input, output, session) {
  real_estate_db <- make_pool()
  # extract the component_fundname
  component_fundname <- DBI::dbGetQuery(real_estate_db, "SELECT
                                          DISTINCT fund_name
                                       FROM nav_allocation
                                       ORDER BY
                                          fund_name;")
  component_fundname <- as.character(component_fundname$fund_name)

  # Your application server logic
  mod_filter_module_server("filters")
  mod_simulator_module_server("simulator")
  mod_fund_exp_filter_server("cf_overview_filter")
  router$server(input, output, session)
}
