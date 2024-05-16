# generate_rabimo_area ---------------------------------------------------------

#' Generate an area in R-ABIMO format with default values.
#' All default values can be overridden by entering new key-value pairs.
#'
#' @param code identifier of area
#' @param \dots key = value pairs overriding the default column values
#' @param column_info data frame as returned by \code{\link{read_column_info}}
#' @export
generate_rabimo_area <- function(code, ..., column_info = read_column_info()) {

  dictionary <- column_info %>%
    dplyr::filter(.data[["type"]] != "berlin-specific") %>%
    select_columns(c("rabimo_berlin", "default", "data_type"))

  arguments <- dictionary$default %>%
    as.list() %>%
    stats::setNames(dictionary$rabimo_berlin)

  is_numeric <- dictionary$data_type == "numeric"
  arguments[is_numeric] <- lapply(arguments[is_numeric], as.numeric)

  result <- kwb.utils::callWith(data.frame, arguments, ...)

  result["code"] <- code

  area_main <- result[["area_main"]]
  total_area <- result[["total_area"]]
  result["main_fraction"] <- round(area_main/total_area , 2)

  roof <- result[["roof"]]
  paved <- result[["pvd"]]
  result["sealed"] <- round(roof + paved, 2)

  result
}
