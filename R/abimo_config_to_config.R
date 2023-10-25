#' Convert Abimo Configuration to List
#'
#' @param abimo_config as returned by \code{kwb.abimo:::read_config}
#' @return list of data frames
#' @export
abimo_config_to_config <- function(abimo_config)
{
  # abimo_config <- kwb.abimo:::read_config()
  #`%>%` <- magrittr::`%>%`

  section_pattern <- "^section_"

  result <- abimo_config %>%
    filter_elements(section_pattern) %>%
    lapply(get_all_item_data)

  names(result) <- gsub(section_pattern, "", names(result))

  element_water <- "Gewaesserverdunstung"
  element_else <- "PotentielleVerdunstung"

  all_columns_to_int <- function(x) as.data.frame(lapply(x, as.integer))

  evap_water <- result %>%
    select_elements(element_water) %>%
    expand_district_ranges() %>%
    all_columns_to_int() %>%
    rename_columns(list(eg = "etp")) %>%
    cbind(etps = -1L, isWaterbody = TRUE)

  evap_else <- result %>%
    select_elements(element_else) %>%
    expand_district_ranges() %>%
    all_columns_to_int() %>%
    cbind(isWaterbody = FALSE)

  result[["potentialEvaporation"]] <- rbind(evap_water, evap_else) %>%
    move_columns_to_front("isWaterbody")

  remove_elements(result, c(element_water, element_else))
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
