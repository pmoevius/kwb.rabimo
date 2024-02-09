# get_usage_tuple --------------------------------------------------------------

#' Get Usage Tuple (Land_type, Veg_class, Irrigation) from NUTZUNG and TYP
#'
#' @param usage value of column NUTZUNG in input data frame
#' @param type value of column TYP in input data frame
#' @param include_inputs logical indicating whether or not to include the
#'   input values in the output
#' @return list with elements \code{land_type}, \code{veg_class}, \code{irrigation}
#' @export
#' @examples
#' get_usage_tuple(10, 10)
#' get_usage_tuple(10, 10, TRUE)
#' get_usage_tuple(10, 1:3)
#' get_usage_tuple(10, 1:3, TRUE)
get_usage_tuple <- function(usage, type, include_inputs = FALSE)
{
  #usage = 10L; type = 10L
  #usage = 10L; type = 333L
  #kwb.utils::assignPackageObjects("kwb.rabimo")

  # Prepare data for which to lookup value combinations in the lookup table
  data <- data.frame(
    berlin_usage = usage,
    berlin_type = type
  )

  result <- as.data.frame(multi_column_lookup(
    data = data,
    lookup = BERLIN_TYPES_TO_USAGE_YIELD_IRRIGATION,
    value = c("land_type", "veg_class", "irrigation"),
    includeKeys = include_inputs
  ))

  if (any(is_missing <- is.na(result[["land_type"]]))) {
    stop_formatted(
      "Could not find a (land_type, veg_class, irrigation) tuple for %s",
      paste(collapse = ", ", sprintf(
        "(NUTZUNG = %d, TYP = %d)",
        usage[is_missing],
        type[is_missing]
      ))
    )
  }

  result
}
