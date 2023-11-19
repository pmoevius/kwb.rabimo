#' Convert Abimo Configuration to List
#'
#' @param abimo_config as returned by \code{kwb.abimo:::read_config}
#' @return list with elements \code{"potential_evaporation"},
#'   \code{"runoff_factors"}, \code{"bagrov_values"}, \code{"diverse"},
#'   \code{"result_digits"}
#' @export

abimo_config_to_config <- function(abimo_config)
{
  #abimo_config <- kwb.abimo:::read_config()
  #`%>%` <- magrittr::`%>%`
  #kwb.utils::assignPackageObjects("kwb.rabimo")

  prefix <- "section_"

  result <- abimo_config[startsWith(names(abimo_config), prefix)] %>%
    lapply(get_all_item_data) %>%
    set_names(remove_left(names(.), nchar(prefix)))

  epot_water <- "Gewaesserverdunstung"
  epot_else <- "PotentielleVerdunstung"

  evaps <- lapply(c(epot_water, epot_else), function(element) {
    result %>%
      select_elements(element) %>%
      expand_district_ranges() %>%
      lapply(as.integer) %>%
      as.data.frame()
  })

  result[["potential_evaporation"]] <- rbind(
    evaps[[1L]] %>%
      rename_columns(list(eg = "etp")) %>%
      cbind(etps = 0, is_waterbody = TRUE),
    evaps[[2L]] %>%
      cbind(is_waterbody = FALSE)
  ) %>%
    move_columns_to_front("is_waterbody")

  convert_element <- function(config, from, to, convert = identity) {
    x <- select_elements(config, from)
    config[[to]] <- set_names(
      convert(select_columns(x, "value")),
      select_columns(x, "key")
    )
    remove_elements(config, from)
  }

  complement <- function(x) 1 - as.numeric(x)

  result <- result %>%
    remove_elements(c(epot_water, epot_else)) %>%
    convert_element("Infiltrationsfaktoren", "runoff_factors", complement) %>%
    convert_element("Bagrovwerte", "bagrov_values", as.numeric) %>%
    convert_element("Diverse", "diverse") %>%
    convert_element("ErgebnisNachkommaStellen", "result_digits", as.numeric)

  # Convert types of "diverse" entries and shift them up to the main level
  diverse <- "diverse"
  x <- select_elements(result, diverse)
  result[["precipitation_correction_factor"]] <- as.numeric(x[["NIEDKORRF"]])
  result[["irrigation_to_zero"]] <- as.logical(x[["BERtoZero"]])
  result <- remove_elements(result, diverse)

  # Clean names for two sections
  keys <- c("runoff_factors", "bagrov_values")
  result[keys] <- lapply(result[keys], function(x) {
    set_names(x, multi_substitute(names(x), list(
      Dachflaechen = "roof",
      Belaglsklasse = "surface"
    )))
  })

  result
}

# get_all_item_data ------------------------------------------------------------
get_all_item_data <- function(x)
{
  get_attribute_data <- function(x) {
    values <- xml2::xml_attrs(x)
    do.call(data.frame, as.list(values))
  }

  get_item_data <- function(x) {
    list_to_data_frame_with_keys(x, "item", "item_(.*)")
  }

  filter_elements(x, "^item_") %>%
    lapply(get_attribute_data) %>%
    get_item_data()
}

# expand_district_ranges -------------------------------------------------------
expand_district_ranges <- function(
    x,
    range_column = "bezirke",
    district_column = "district"
)
{
  x <- x %>%
    expand_ranges_in_column(range_column, district_column) %>%
    reset_row_names() %>%
    remove_columns(c("item", range_column)) %>%
    move_columns_to_front(district_column)

  stopifnot(
    !anyDuplicated(select_columns(x, district_column))
  )

  x
}
