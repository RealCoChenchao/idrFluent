#' preload_data
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
#' @import dplyr

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

# Net total return & Gross total return
odce_returns <- tbl(real_estate_db, "odce_returns") %>%
  dplyr::rename(quarter = period) %>%
  dplyr::collect()
