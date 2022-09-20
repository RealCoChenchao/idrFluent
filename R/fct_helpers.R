#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import leaflet.extras
#' @import tidyr
#' @import scales
#' @import zoo
#' @noRd
#' @export

# summarize the metrics based on nav and input weight
calc_weight <- function(input_dt,
                        wt,
                        group_var = NULL){
  prep_data <- input_dt %>%
    dplyr::inner_join(wt,
                      by = c("fund_name")) %>%
    group_by({{ group_var }})

  prep_data %>%
    dplyr::summarise(
      across(contains("total"),
             ~weighted.mean(.,input_weight))) %>%
    dplyr::mutate(
      across(contains("total"),
             ~round(.x, digits = 3))) %>%
    dplyr::mutate(fund_name = "Portfolio") %>%
    dplyr::bind_rows(
      input_dt %>%
        dplyr::filter(fund_name == "ODCE") %>%
        dplyr::select(fund_name,
                      {{ group_var }},
                      contains("total"))
    )
}

# custimized format function for return, std and track error tables
wider_return_sdtr <- function(df, fct_levels){
  df %>%
    tidyr::pivot_longer(cols = contains("total"),
                        names_to = "variable",
                        values_to = "total_pct") %>%
    dplyr::mutate(period = str_extract(variable, "[0-9]+"),
                  period = as.integer(period),
                  variable = str_extract(variable, "[a-z]+\\_[a-z]+"),
                  year = paste0(period, "-Yr"),
                  year = factor(year, levels = paste0(fct_levels, "-Yr"))) %>%
    dplyr::filter(!is.na(period)) %>%
    tidyr::pivot_wider(id_cols = c("fund_name", "period", "year"),
                       names_from = variable,
                       values_from = total_pct)
}
