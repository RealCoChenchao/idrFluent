#' preload_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import dplyr
#' @import zoo

### get the IDR fund information from the DB
real_estate_db <- pool::dbPool(
  drv      = RPostgres::Postgres(),
  dbname   = Sys.getenv("AWS_IDR_DATABASE"),
  host     = Sys.getenv("AWS_IDR_SERVER"),
  port     = Sys.getenv("AWS_IDR_PORT"),
  user     = Sys.getenv("AWS_IDR_USERNAME"),
  password = Sys.getenv("AWS_IDR_PASSWORD")
)

### NAV value
cf_fund_level <- tbl(real_estate_db, "cf_fund_level") %>%
  dplyr::filter(fund_level_data == "Net Assets Total",
                quarter == max(quarter, na.rm = TRUE)) %>%
  dplyr::select(fund_name, nav = value) %>%
  dplyr::collect()

# extract the component_fundname
component_fundname <- DBI::dbGetQuery(real_estate_db, "SELECT
                                          DISTINCT fund_name
                                       FROM nav_allocation
                                       ORDER BY
                                          fund_name;")
component_fundname <- as.character(component_fundname$fund_name)

# read the fund nav from the table
fund_nav <- tbl(real_estate_db,
                "cf_fund_level") %>%
  dplyr::filter(
    fund_level_data == "Net Assets Total",
    quarter == max(quarter, na.rm = TRUE)) %>%
  dplyr::select(fund_name, total_nav = value) %>%
  dplyr::collect() %>%
  dplyr::bind_rows(
    tbl(real_estate_db,
        "odce_returns") %>%
      dplyr::filter(period == max(period),
                    fund_name == "ODCE") %>%
      dplyr::select(fund_name,
                    total_nav = net_asset_value) %>%
      dplyr::collect()
  )

# read the ODCE and all component fund quarterly and historical returns
fund_return <- tbl(real_estate_db,
                   "odce_returns") %>%
  dplyr::select(quarter = period,
                fund_name,
                contains("total_return")) %>%
  dplyr::collect()

fund_latest_return <- dplyr::filter(fund_return, quarter == max(quarter))

summarized_return <- tbl(real_estate_db,  "ps_summarized_return") %>%
  dplyr::collect()

# read the ODCE and all component fund sector diversification
fund_diversification <- tbl(real_estate_db,  "odce_propertytype_diverfication") %>%
  dplyr::filter(quarter == max(quarter)) %>%
  dplyr::mutate(
    fund_name = ifelse(
      fund_name == "NFI-ODCE",
      "ODCE",
      fund_name
    )
  ) %>%
  dplyr::select(-quarter) %>%
  dplyr::collect()

# get the ODCE and component fund leverage
processed_odce_history <- tbl(real_estate_db, "processed_odce_history") %>%
  dplyr::filter(quarter == max(quarter, na.rm = TRUE)) %>%
  dplyr::select(quarter, total_leverage) %>%
  dplyr::mutate(fund_name = "ODCE") %>%
  dplyr::collect()

fund_leverage <- tbl(real_estate_db,
                     "cf_fund_level") %>%
  dplyr::filter(
    quarter == max(quarter, na.rm = TRUE),
    fund_level_data == c("Total Leverage %")) %>%
  dplyr::collect() %>%
  spread(fund_level_data, value) %>%
  janitor::clean_names() %>%
  dplyr::rename(
    total_leverage = total_leverage_percent
  ) %>%
  dplyr::bind_rows(processed_odce_history) %>%
  dplyr::ungroup()
