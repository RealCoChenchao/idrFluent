#' visual_helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import leaflet.extras
#' @import tidyr
#' @import scales
#' @noRd

# elements for leaflet maps
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

# dodge plot function for portfolio simulator
ps_dodge_plot <- function(df, x_value, y_value){
  df %>%
    ggplot(aes(x = .data[[x_value]],
               y = .data[[y_value]],
               fill = fund_name,
               label = scales::percent(.data[[y_value]], accuracy = 0.1))) +
    geom_bar(position="dodge",stat="identity") +
    scale_fill_manual(values = c("#12395b", "#a2a4a3")) +
    scale_y_continuous(labels = scales::percent) +
    geom_text(position = position_dodge(width = 0.9),
              hjust =.5,
              vjust = -0.5,
              size = 3) +
    labs(x = "", y ="", fill = "") +
    theme_minimal() +
    theme(axis.text.y =
            element_text(size = 14,
                         face = "bold"),
          axis.text.x =
            element_text(size = 14,
                         face = "bold"))
}
