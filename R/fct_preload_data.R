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

# extract the component_fundname
component_fundname <- DBI::dbGetQuery(real_estate_db, "SELECT
                                          DISTINCT fund_name
                                       FROM nav_allocation
                                       ORDER BY
                                          fund_name;")
component_fundname <- as.character(component_fundname$fund_name)

# read the fund nav from the table
fund_nav <- tbl(real_estate_db,
                "radar_odce_performance")  %>%
  dplyr::filter(quarter == max(quarter)) %>%
  dplyr::select(quarter, fund_name, total_nav = net_asset_value) %>%
  dplyr::collect()


# get the ODCE and component fund leverage
fund_leverage <- tbl(real_estate_db, "radar_odce_performance") %>%
  dplyr::filter(quarter == max(quarter, na.rm = TRUE)) %>%
  dplyr::select(quarter, fund_name, total_leverage) %>%
  dplyr::collect()

# read the ODCE and all component fund quarterly and historical returns
fund_latest_return <- tbl(real_estate_db,
                          "radar_odce_performance") %>%
  dplyr::filter(quarter == max(quarter, na.rm = TRUE)) %>%
  dplyr::select(quarter, fund_name, contains("total_return")) %>%
  dplyr::collect()

summarized_return <- tbl(real_estate_db,  "ps_summarized_return") %>%
  dplyr::collect()

# read the ODCE and all component fund sector diversification
fund_diversification <- tbl(real_estate_db, "radar_odce_divf") %>%
  dplyr::filter(quarter == max(quarter),
                diverf_cat == "property_type",
                !is.na(div_pct)) %>%
  dplyr::select(fund_name,
                diversification,
                total_pct = div_pct) %>%
  dplyr::collect()

