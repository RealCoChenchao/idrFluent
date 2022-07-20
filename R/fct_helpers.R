#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import leaflet.extras
#' @import tidyr
#' @import scales
#' @noRd

icons <- awesomeIconList(
  Office = makeAwesomeIcon(icon = 'building',
                           library = 'fa',
                           markerColor = "orange"),
  Industrial = makeAwesomeIcon(icon = 'building',
                               library = 'fa',
                               markerColor = "lightgreen"),
  Apartment = makeAwesomeIcon(icon = 'building',
                                library = 'fa',
                                markerColor = "lightblue"),
  Retail = makeAwesomeIcon(icon = 'building',
                           library = 'fa',
                           markerColor = "purple"),
  Other = makeAwesomeIcon(icon = 'building',
                          library = 'fa',
                          markerColor = "beige")
)

addPortfolioMarkers <- function(map, df, selected_sector, cluster_option){

  if (selected_sector == "Other"){
    selected_data <- df %>%
      dplyr::filter(!property_type %in% c("Office", "Industrial",
                                          "Apartment", "Retail"))
  }else{
    selected_data <- dplyr::filter(df, property_type == selected_sector)
  }

  if (dim(selected_data)[1] != 0) {
      addAwesomeMarkers(map,
                        data = selected_data,
                        group = selected_sector,
                        icon = ~icons[selected_sector],
                        label = ~investment_name,
                        popup = ~paste("Investment: ", replace_na(investment_name, ""), "<br>",
                                       "Address: ",replace_na(street_address, ""), "<br>",
                                       "Fund Name: ", fund_name, "<br>",
                                       replace_na(city, ""), replace_na(state, ""), replace_na(zip_code, ""), "<br>",
                                       "SF: ", comma(square_feet), "<br>",
                                       "Property Type: ",  "<b>", replace_na(as.character(property_type), ""), "</b>", "<br>",
                                       "Total GAV: ", scales::dollar(total_gav), "<br>",
                                       "Fund GAV: ", scales::dollar(fund_gav), "<br>",
                                       "Life Cycle: ", replace_na(as.character(property_life_cycle), ""), "<br>",
                                       "Occupany: ", scales::percent(occupancy), "<br>",
                                       "Cap Rate: ", scales::percent(current_cap_rate), "<br>"),
                        clusterOptions = ifelse(cluster_option == 1, markerClusterOptions(), NA))

  } else {
    map
  }
}

