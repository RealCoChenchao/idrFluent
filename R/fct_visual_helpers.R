#' visual_helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import leaflet.extras
#' @import tidyr
#' @import scales
#' @import officer
#' @import ggExtra
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
# ps_dodge_plot <- function(df, x_value, y_value){
#   df %>%
#     ggplot(aes(x = .data[[x_value]],
#                y = .data[[y_value]],
#                fill = fund_name,
#                label = scales::percent(.data[[y_value]], accuracy = 0.1))) +
#     geom_bar(position="dodge",stat="identity") +
#     scale_fill_manual(values = c("#12395b", "#a2a4a3")) +
#     scale_y_continuous(labels = scales::percent) +
#     geom_text(position = position_dodge(width = 0.9),
#               hjust =.5,
#               vjust = -0.5,
#               size = 3) +
#     labs(x = "", y ="", fill = "") +
#     theme_minimal() +
#     theme(axis.text.y =
#             element_text(size = 14,
#                          face = "bold"),
#           axis.text.x =
#             element_text(size = 14,
#                          face = "bold"))
# }

ps_dodge_plot_v2 <- function(df, variable, value, fill_category = TRUE,
                             label_accuracy = 0.1, x_text_angle = 0,
                             label_text_size = 3.5, axis_text_size = 12){

  standard_name <- c("Portfolio", "ODCE")

  if(x_text_angle == 0){
    x_text_hjust <- NULL
    x_text_vjust <- NULL

  }else{
    x_text_hjust <- 1
    x_text_vjust <- 1
  }

  df <- df %>%
    dplyr::select(name = fund_name, variable = rlang::sym(variable), value = rlang::sym(value))

  if(fill_category){
    df <- expand.grid(name = unique(df$name), variable = unique(df$variable)) %>%
      left_join(df, by = c("name", "variable"))
  }

  df <- df %>%
    mutate(label_text = scales::percent(value, accuracy = label_accuracy)) %>%
    mutate(vjust = ifelse(label_text < 0, 1.5, -0.5)) %>%
    mutate(name = factor(name, levels = standard_name[standard_name %in% df$name]))

  if(variable == "fund_name"){
    df <- df %>%
      mutate(variable = factor(variable, levels = c("Portfolio", "ODCE")))
  }else if(variable == "diversification"){
    df$variable <- factor(df$variable, levels = df[df$name != "ODCE",]$variable[order(df[df$name != "ODCE",]$value, decreasing = TRUE)], ordered = TRUE)
  }

  seq <- pretty(c(df$value, 0), 8)
  breaks <- seq[2] - seq[1]

  if(breaks < 0.01){
    label_scale <- scales::percent_format(accuracy = 0.1)
  }else{
    label_scale <- scales::percent_format(accuracy = 1)
  }

  if(max(seq) > 0){
    seq <- c(seq, max(seq) + breaks)
  }

  if(min(seq) < 0){
    seq <- c(min(seq) - breaks, seq)
  }

  if(length(levels(df$name)) == 1){
    legend_color <- setNames(c("#1D2EA7"), levels(df$name))
  }else{
    legend_color <- setNames(c("#1D2EA7", "#24BCFF"), levels(df$name))
  }

  p <- df %>%
    ggplot(aes(x = variable, y = value, fill = name, label = label_text, vjust = vjust)) +
    geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.8)) +
    geom_text(size = label_text_size, color = "#444444", position = position_dodge(width = 0.8)) +
    scale_fill_manual(name = NULL, values = legend_color) +
    scale_y_continuous(labels = label_scale,
                       breaks = seq,
                       limits = c(min(seq), max(seq))) +
    ggExtra::removeGrid(x = TRUE, y = TRUE) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = axis_text_size, color = "#444444", family = "Helvetica",
                                     angle = x_text_angle, vjust = x_text_vjust, hjust = x_text_hjust),
          axis.text.y = element_text(size = axis_text_size, color = "#444444", family = "Helvetica"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_line(color = "#D9D9D9", size = 0.2),
          legend.position = "bottom",
          legend.text = element_text(size = axis_text_size, color = "#444444", family = "Helvetica"),
          legend.key.height = unit(4, 'mm'),
          legend.key.width = unit(4, 'mm')) +
    geom_hline(yintercept = 0, size = 0.2)

  p
}

ps_fund_weight_plot <- function(df){
  df <- df %>%
    dplyr::select(name = fund_name, value = input_weight) %>%
    mutate(label_text = scales::percent(value, accuracy = 0.1)) %>%
    mutate(vjust = ifelse(label_text < 0, 1.5, -0.5))

  seq <- pretty(c(df$value, 0), 5)
  breaks <- seq[2] - seq[1]

  if(breaks < 0.01){
    label_scale <- scales::percent_format(accuracy = 0.1)
  }else{
    label_scale <- scales::percent_format(accuracy = 1)
  }

  if(max(seq) > 0){
    seq <- c(seq, max(seq) + breaks)
  }

  if(min(seq) < 0){
    seq <- c(min(seq) - breaks, seq)
  }

  if(dim(df)[1] <= 8){
    x_text_angle <- 0
    x_text_hjust <- NULL
    x_text_vjust <- NULL
  }else{
    x_text_angle <- 60
    x_text_hjust <- 1
    x_text_vjust <- 1
  }

 p <- df %>%
    ggplot(aes(x = reorder(name, -value), y = value, label = label_text, vjust = vjust)) +
    geom_bar(stat = "identity", width = 0.6, position = position_dodge(width = 0.8), fill = "#1D2EA7") +
    geom_text(size = 3.5, color = "#444444", position = position_dodge(width = 0.8)) +
    scale_y_continuous(labels = label_scale,
                       breaks = seq,
                       limits = c(min(seq), max(seq))) +
    ggExtra::removeGrid(x = TRUE, y = TRUE) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_text(size = 12, color = "#444444", family = "Helvetica",
                                     angle = x_text_angle, vjust = x_text_vjust, hjust = x_text_hjust),
          axis.text.y = element_text(size = 12, color = "#444444", family = "Helvetica"),
          panel.background = element_rect(fill = "transparent"),
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_line(color = "#D9D9D9", size = 0.2),
          legend.position = "bottom",
          legend.text = element_text(size = 12, color = "#444444", family = "Helvetica"),
          legend.key.height = unit(4, 'mm'),
          legend.key.width = unit(4, 'mm')) +
    geom_hline(yintercept = 0, size = 0.2)

 p
}

fund_exp_diverf_plot <- function(df){
  ggplot(data = df) +
    geom_col(aes(x = reorder(diversification, -div_pct),
                 y = div_pct),
             fill = "#05D0EB") +
    scale_y_continuous(labels = scales::percent,
                       breaks = seq(0, 1, 0.05),
                       limits = c(0, NA)) +
    labs(x = "", y = "") +
    theme_minimal() +
    theme(axis.text.y =
            element_text(size = 14,
                         face = "bold"),
          axis.text.x =
            element_text(size = 14,
                         face = "bold"))
}

# Slides functions
disclaimer <- "For Institutional Use Only. Not for Public Distribution. Highly Confidential – Trade Secret and Patented"
title_font <- officer::fp_text(font.size = 40, bold = TRUE, color = "#444444", font.family = "Apex Sans Bold", underlined = FALSE)
footnote_font <- officer::fp_text(font.size = 10, bold = FALSE, color = "#707070", font.family = "Gotham Narrow Book", underlined = FALSE)
page_number_font <- officer::fp_text(font.size = 10, bold = TRUE, color = "#A1A1A1", font.family = "Apex Sans Bold", underlined = FALSE)

clear_template <- function(template){
  len = length(template)
  for (n in len:1) {
    remove_slide(template, n)
  }
  return(template)
}

gen_title_footnote_disclaimer_slide <- function(template, title, footnote){
  add_slide(template, layout = "085", master = layout_summary(template)$master[1]) %>%
    ph_with(value = fpar(ftext(title, title_font)),
            location = ph_location(left = 0.7, top = 0.3, width = 12, height = 0.8)) %>%
    ph_with(value = fpar(ftext(footnote, footnote_font)),
            location = ph_location(left = 0, top = 7.25, width = 12, height = 0.3)) %>%
    ph_with(value = external_img(src = "slide_template/line.png"),
            location = ph_location(left = 12.85, top = 6.93, width = 0.01, height = 0.44)) %>%
    ph_with(value = fpar(ftext(disclaimer), fp_p = fp_par(text.align = "right"), fp_t = title_font),
            location = ph_location(left = 9.15, top = 6.93, width = 3.7, height = 0.44))
}

generate_slides <- function(data, fund_weight){
  as_of_date <- data$quarter
  as_of_date <- paste0(str_sub(as_of_date, 7L, 7L), "Q ", str_sub(as_of_date, 1L, 4L))

  template <- read_pptx("slide_template/template.pptx")
  template <- clear_template(template)

  template <- gen_title_footnote_disclaimer_slide(template, "Portfolio Composition",
                                                  paste0("Source: IDR. Data as of ", as_of_date, ".")) %>%
    ph_with(value = ps_fund_weight_plot(fund_weight),
            location = ph_location(left = 0.6, top = 1.18, width = 12.13, height = 5.77))

  template <- gen_title_footnote_disclaimer_slide(template, "Net Total Return (Annualized)",
                                                  paste0("Source: IDR. Data as of ", as_of_date, ".")) %>%
    ph_with(value = ps_dodge_plot_v2(df = data$calc_return, variable = "year", value = "net_total"),
            location = ph_location(left = 0.6, top = 1.18, width = 12.13, height = 5.77))

  template <- gen_title_footnote_disclaimer_slide(template, "Gross Total Return (Annualized)",
                                                  paste0("Source: IDR. Data as of ", as_of_date, ".")) %>%
    ph_with(value = ps_dodge_plot_v2(df = data$calc_return, variable = "year", value = "gross_total"),
            location = ph_location(left = 0.6, top = 1.18, width = 12.13, height = 5.77))

  template <- gen_title_footnote_disclaimer_slide(template, "Property Type Diversification",
                                                  paste0("Source: IDR. Data as of ", as_of_date, ".")) %>%
    ph_with(value = ps_dodge_plot_v2(df = data$calc_diversification, variable = "diversification", value = "total_pct"),
            location = ph_location(left = 0.6, top = 1.18, width = 12.13, height = 5.77))

  template <- gen_title_footnote_disclaimer_slide(template, "Portfolio Leverage",
                                                  paste0("Source: IDR. Data as of ", as_of_date, ".")) %>%
    ph_with(value = ps_dodge_plot_v2(df = data$calc_leverage, variable = "fund_name", value = "total_leverage", fill_category = FALSE),
            location = ph_location(left = 0.6, top = 1.18, width = 12.13, height = 5.77))

  template <- gen_title_footnote_disclaimer_slide(template, "Standard Deviation",
                                                  paste0("Source: IDR. Data as of ", as_of_date, ".")) %>%
    ph_with(value = ps_dodge_plot_v2(df = data$calc_sdtr, variable = "year", value = "total_std"),
            location = ph_location(left = 0.6, top = 1.18, width = 12.13, height = 5.77))

  template <- gen_title_footnote_disclaimer_slide(template, "Tracking Error to ODCE",
                                                  paste0("Source: IDR. Data as of ", as_of_date, ".")) %>%
    ph_with(value = ps_dodge_plot_v2(df = dplyr::filter(data$calc_sdtr, fund_name != "ODCE"), variable = "year", value = "total_te"),
            location = ph_location(left = 0.6, top = 1.18, width = 12.13, height = 5.77))

  n_slides <- length(template)
  for (i_slide in 1:n_slides) {
    template <- template %>%
      on_slide(index = i_slide) %>%
      ph_with(value = fpar(ftext(i_slide, page_number_font)),
              location = ph_location(left = 12.9, top = 7, width = 0.4, height = 0.4))
  }

  template
}
