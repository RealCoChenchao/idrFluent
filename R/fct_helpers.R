#' helpers
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#' @import leaflet.extras
#' @import tidyr
#' @import scales
#' @noRd
#' @export

# build return comparison for portfolio and ODCE
comp_return <- function(odce_returns, fund_weight){

  comp_return <- odce_returns %>%
    dplyr::select(fund_name, contains("hist")) %>%
    dplyr::select(fund_name, contains("total")) %>%
    dplyr::inner_join(fund_weight,
                      by = c("fund_name")) %>%
    dplyr::summarise(across(contains("total"),
                            ~weighted.mean(.,input_weight))) %>%
    dplyr::mutate(across(contains("total"),
                         ~round(.x, digits = 3))) %>%
    dplyr::mutate(fund_name = "Portfolio") %>%
    dplyr::bind_rows(odce_returns %>%
                       dplyr::select(fund_name, contains("hist")) %>%
                       dplyr::select(fund_name, contains("total")) %>%
                       dplyr::filter(fund_name == "ODCE")) %>%
    dplyr::select(fund_name, contains("net")) %>%
    tidyr::pivot_longer(cols = contains("net"),
                        names_to = "variable",
                        values_to = "value") %>%
    dplyr::mutate(vintage = as.integer(str_extract(variable, "[0-9]+")),
                  vintage_group = paste0(vintage, "-Yr"))

  comp_return
}

