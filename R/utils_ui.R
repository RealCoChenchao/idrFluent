#' ui
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd

# enframe(available_geoid_lookup_table) %>%
#   dplyr::mutate(value = unlist(value)) %>%
#   dplyr::mutate(
#     json_str = glue::glue('list(key = "{value}", text = "{name}")')
#   ) %>%
#   dplyr::pull(json_str) %>%
#   write.table(file = "input.txt",
#               quote = FALSE, row.names = FALSE, col.names = FALSE)

# tidy_sf %>%
#   dplyr::pull(fund_name) %>%
#   unique() %>%
#   tibble::enframe() %>%
#   dplyr::mutate(
#     json_str = glue::glue('list(key = "{value}", text = "{value}")')
#   ) %>%
#   dplyr::pull(json_str) %>%
#   write.table(file = "input.txt",
#               quote = FALSE, row.names = FALSE, col.names = FALSE)


details_list_columns <- tibble(
  fieldName = c("fund_name",
                "investment_name",
                "year_built",
                "square_feet",
                "property_type",
                "NAME",
                "city",
                "occupancy",
                "property_life_cycle"),
  name = c("Fund Name",
           "Investment Name",
           "Year Built",
           "Area",
           "Property Type",
           "MSA",
           "City",
           "Occupancy",
           "Life Cycle"),
  key = fieldName)

# input options
metro_options <- list(
  list(key = "408", text = "New York-Newark-Jersey City, NY-NJ-PA"),
  list(key = "348", text = "Los Angeles–Long Beach–Anaheim, CA"),
  list(key = "176", text = "Chicago-Naperville-Elgin, IL-IN-WI"),
  list(key = "548", text = "Washington-Baltimore-Arlington, DC-MD-VA-WV-PA"),
  list(key = "488", text = "San Jose–San Francisco–Oakland, CA"),
  list(key = "148", text = "Boston-Worcester-Providence, MA-RI-NH-CT"),
  list(key = "206", text = "Dallas-Fort Worth, TX"),
  list(key = "428", text = "Philadelphia-Camden-Vineland, PA-NJ-DE-MD"),
  list(key = "288", text = "Houston-Baytown-Huntsville, TX"),
  list(key = "370", text = "Miami–Fort Lauderdale–West Palm Beach, FL"),
  list(key = "122", text = "Atlanta-Athens-Clarke County-Sandy Springs, GA"),
  list(key = "220", text = "Detroit-Warren-Ann Arbor, MI"),
  list(key = "500", text = "Seattle-Tacoma, WA"),
  list(key = "38060", text = "Phoenix-Mesa-Scottsdale, AZ"),
  list(key = "378", text = "Minneapolis-St. Paul, MN-WI"),
  list(key = "184", text = "Cleveland-Akron-Canton, OH"),
  list(key = "216", text = "Denver-Aurora, CO"),
  list(key = "41740", text = "San Diego-Carlsbad, CA"),
  list(key = "422", text = "Orlando-Deltona-Daytona Beach, FL"),
  list(key = "440", text = "Portland-Vancouver-Salem, OR-WA"),
  list(key = "45300", text = "Tampa-St. Petersburg-Clearwater, FL"),
  list(key = "476", text = "St. Louis-St. Charles-Farmington, MO-IL"),
  list(key = "430", text = "Pittsburgh-New Castle-Weirton, PA-OH-WV"),
  list(key = "172", text = "Charlotte-Concord, NC-SC"),
  list(key = "472", text = "Sacramento-Roseville, CA"),
  list(key = "482", text = "Salt Lake City-Provo-Orem, UT"),
  list(key = "312", text = "Kansas City-Overland Park-Kansas City, MO-KS"),
  list(key = "198", text = "Columbus-Marion-Zanesville, OH"),
  list(key = "41700", text = "San Antonio-New Braunfels, TX"),
  list(key = "332", text = "Las Vegas-Henderson, NV-AZ"),
  list(key = "294", text = "Indianapolis-Carmel-Muncie, IN"),
  list(key = "178", text = "Cincinnati-Wilmington-Maysville, OH-KY-IN"),
  list(key = "450", text = "Raleigh-Durham-Chapel Hill, NC"),
  list(key = "12420", text = "Austin-Round Rock, TX"),
  list(key = "376", text = "Milwaukee-Racine-Waukesha, WI"),
  list(key = "400", text = "Nashville-Davidson--Murfreesboro, TN"),
  list(key = "545", text = "Virginia Beach-Norfolk, VA-NC"),
  list(key = "268", text = "Greensboro--Winston-Salem--High Point, NC"),
  list(key = "300", text = "Jacksonville-St. Marys-Palatka, FL-GA"),
  list(key = "350", text = "Louisville/Jefferson County--Elizabethtown--Madison, KY-IN"),
  list(key = "406", text = "New Orleans-Metairie-Hammond, LA-MS"),
  list(key = "278", text = "Hartford-West Hartford, CT"),
  list(key = "416", text = "Oklahoma City-Shawnee, OK"),
  list(key = "266", text = "Grand Rapids-Wyoming-Muskegon, MI"),
  list(key = "273", text = "Greenville-Spartanburg-Anderson, SC"),
  list(key = "368", text = "Memphis-Forrest City, TN-MS-AR"),
  list(key = "142", text = "Birmingham-Hoover-Talladega, AL"),
  list(key = "40060", text = "Richmond, VA"),
  list(key = "276", text = "Harrisburg-York-Lebanon, PA"),
  list(key = "160", text = "Buffalo-Cheektowaga, NY"),
  list(key = "106", text = "Albuquerque-Santa Fe-Las Vegas, NM"),
  list(key = "104", text = "Albany-Schenectady, NY"),
  list(key = "464", text = "Rochester-Batavia-Seneca Falls, NY"),
  list(key = "538", text = "Tulsa-Muskogee-Bartlesville, OK"),
  list(key = "260", text = "Fresno-Madera, CA"),
  list(key = "314", text = "Knoxville-Morristown-Sevierville, TN"),
  list(key = "162", text = "Cape Coral-Fort Myers-Naples, FL"),
  list(key = "212", text = "Dayton-Springfield-Sidney, OH"),
  list(key = "536", text = "Tucson-Nogales, AZ"),
  list(key = "238", text = "El Paso-Las Cruces, TX-NM"),
  list(key = "412", text = "North Port-Sarasota, FL"),
  list(key = "420", text = "Omaha-Council Bluffs-Fremont, NE-IA"),
  list(key = "192", text = "Columbia-Orangeburg-Newberry, SC"),
  list(key = "365", text = "McAllen-Edinburg, TX"),
  list(key = "12540", text = "Bakersfield, CA"),
  list(key = "357", text = "Madison-Janesville-Beloit, WI"),
  list(key = "12940", text = "Baton Rouge, LA"),
  list(key = "382", text = "Modesto-Merced, CA"),
  list(key = "218", text = "Des Moines-Ames-West Des Moines, IA"),
  list(key = "147", text = "Boise City-Mountain Home-Ontario, ID-OR"),
  list(key = "16700", text = "Charleston-North Charleston, SC"),
  list(key = "515", text = "South Bend-Elkhart-Mishawaka, IN-MI"),
  list(key = "17820", text = "Colorado Springs, CO"),
  list(key = "518", text = "Spokane-Spokane Valley-Coeur d'Alene, WA-ID"),
  list(key = "170", text = "Charleston-Huntington-Ashland, WV-OH-KY"),
  list(key = "556", text = "Wichita-Arkansas City-Winfield, KS"),
  list(key = "29460", text = "Lakeland-Winter Haven, FL"),
  list(key = "566", text = "Youngstown-Warren, OH-PA"),
  list(key = "534", text = "Toledo-Port Clinton, OH"),
  list(key = "456", text = "Reno-Carson City-Fernley, NV"),
  list(key = "42540", text = "Scranton--Wilkes-Barre--Hazleton, PA"),
  list(key = "496", text = "Savannah-Hinesville-Statesboro, GA"),
  list(key = "29540", text = "Lancaster, PA"),
  list(key = "533", text = "Tallahassee-Bainbridge, FL-GA"),
  list(key = "33860", text = "Montgomery, AL")
)


fund_options <- list(
  list(key = "AEW", text = "AEW"),
  list(key = "Ara", text = "Ara"),
  list(key = "ASB", text = "ASB"),
  list(key = "Barings", text = "Barings"),
  list(key = "BentallGreenOak", text = "BentallGreenOak"),
  list(key = "BlackRock", text = "BlackRock"),
  list(key = "CBRE", text = "CBRE"),
  list(key = "Clarion", text = "Clarion"),
  list(key = "DWS", text = "DWS"),
  list(key = "Heitman", text = "Heitman"),
  list(key = "Intercontinental", text = "Intercontinental"),
  list(key = "INVESCO", text = "INVESCO"),
  list(key = "JP Morgan", text = "JP Morgan"),
  list(key = "LaSalle", text = "LaSalle"),
  list(key = "MetLife", text = "MetLife"),
  list(key = "Morgan Stanley", text = "Morgan Stanley"),
  list(key = "PGIM", text = "PGIM"),
  list(key = "PNC AFL-CIO", text = "PNC AFL-CIO"),
  list(key = "Principal", text = "Principal"),
  list(key = "Stockbridge", text = "Stockbridge"),
  list(key = "TA Realty", text = "TA Realty"),
  list(key = "UBS", text = "UBS"))

fund_names <- unlist(purrr::map(fund_options, ~.x$key))

sector_options <- list(
  list(key = "Apartment", text = "Apartment"),
  list(key = "Industrial", text = "Industrial"),
  list(key = "Office", text = "Office"),
  list(key = "Retail", text = "Retail"),
  list(key = "Land", text = "Land"),
  list(key = "Self-Storage", text = "Self-Storage"),
  list(key = "Other", text = "Other"),
  list(key = "Healthcare", text = "Healthcare"),
  list(key = "Student Housing", text = "Student Housing"),
  list(key = "Senior Living", text = "Senior Living"),
  list(key = "Hotel", text = "Hotel"),
  list(key = "Parking", text = "Parking"))

quarter_options <- list(
  list(key = "2021 Q1", text = "2021 Q1"),
  list(key = "2021 Q2", text = "2021 Q2"),
  list(key = "2021 Q3", text = "2021 Q3"))

ps_options <- list(
  list(key = "net_total", text = "Net Return (Annualized)"),
  list(key = "gross_total", text = "Gross Return (Annualized)"),
  list(key = "property_type", text = "Property Type Diversification"),
  list(key = "total_leverage", text = "Portfolio Leverage"),
  list(key = "total_std", text = "Standard Deviation"),
  list(key = "total_te", text = "Tracking Error to ODCE"))

diverf_options <- list(
  list(key = "property_type", text = "Property Type"),
  list(key = "region", text = "Region"),
  list(key = "structure", text = "Structure"),
  # list(key = "valuation", text = "Valuation"),
  list(key = "lifecycle", text = "Lifecycle"))

makePage <- function (title, subtitle, contents) {
  tagList(div(
    class = "page-title",
    span(title, class = "ms-fontSize-32 ms-fontWeight-semibold", style =
           "color: #323130"),
    span(subtitle, class = "ms-fontSize-14 ms-fontWeight-regular", style =
           "color: #605E5C; margin: 14px;")
  ),
  contents)
}

makeCard <- function(title, content, size = 12, style = "") {
  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      Text(variant = "large", title, block = TRUE),
      content
    )
  )
}

makesimpleCard <- function(content, size = 12, style = "") {
  div(
    class = glue("card ms-depth-8 ms-sm{size} ms-xl{size}"),
    style = style,
    Stack(
      tokens = list(childrenGap = 5),
      content
    )
  )
}

header <- tagList(
  img(src = "www/idr_logo.png", class = "logo"),
  CommandBar(
    farItems = list(
      CommandBarItem("Grid view", "Tiles", iconOnly = TRUE),
      CommandBarItem("Info", "Info", iconOnly = TRUE)
    ),
    style = list(width = "100%")))

navigation <- Nav(
  groups = list(
    list(links = list(
      list(name = 'Home', url = '#!/', key = 'home', icon = 'Home'),
      list(name = 'Query Tool', url = '#!/other', key = 'querytool', icon = 'BIDashboard'),
      list(name = 'Component Fund Overview', url = '#!/cf_overview', key = 'cf_overview', icon = 'BIDashboard'),
      list(name = 'Portfolio Simulator', url = '#!/ps', key = 'simulator', icon = 'FunnelChart'),
      list(name = 'IDR', url = 'https://idrinvestments.com', key = 'idr', icon = 'WebComponents'),
      list(name = 'USAA Real Estate', url = 'https://www.usrealco.com/', key = 'realco', icon = 'WebAppBuilderFragment')
    ))
  ),
  initialSelectedKey = 'home',
  styles = list(
    root = list(
      height = '100%',
      boxSizing = 'border-box',
      overflowY = 'auto'
    )
  )
)

footer <- Stack(
  horizontal = TRUE,
  horizontalAlign = 'space-between',
  tokens = list(childrenGap = 20),
  Text(variant = "medium", "Built with ❤ by Chenchao Zang", block=TRUE),
  Text(variant = "medium", nowrap = FALSE, "If you'd like to learn more, reach out to us at chenchao.zang@usrealco.com"),
  Text(variant = "medium", nowrap = FALSE, "All rights reserved.")
)


layout <- function(mainUI){
  div(class = "grid-container",
      div(class = "header", header),
      div(class = "sidenav", navigation),
      div(class = "main", mainUI),
      div(class = "footer", footer)
  )
}

querytool_page <- makePage(
  "Component fund query tool",
  "Best fund level details",
  div(
    Stack(
      horizontal = TRUE,
      tokens = list(childrenGap = 10),
      mod_filter_module_ui("filters")
    )
  )
)

card1 <- makeCard(
  "Welcome to shiny.fluent demo!",
  div(
    Text("shiny.fluent is a package that allows you to build Shiny apps using Microsoft's Fluent UI."),
    Text("Use the menu on the left to explore live demos of all available components.")
  ))

home_page <- makePage(
  "This is a Fluent UI app built in Shiny",
  "shiny.react + Fluent UI = shiny.fluent",
  div(card1)
)

simulator_page <- makePage(
  "Portfolio simulator",
  "Try your best interests",
  div(
    mod_simulator_module_ui("simulator")
  )
)

cf_overview_page <- makePage(
  "Component fund overview",
  "One screen for everything",
  div(
    mod_fund_exp_filter_ui("cf_overview_filter")
  )
)
router <- make_router(
  route("/", home_page),
  route("other", querytool_page),
  route("ps", simulator_page),
  route("cf_overview", cf_overview_page))

shiny::addResourcePath("shiny.router", system.file("www", package = "shiny.router"))
shiny_router_js_src <- file.path("shiny.router", "shiny.router.js")
shiny_router_script_tag <- shiny::tags$script(type = "text/javascript", src = shiny_router_js_src)
