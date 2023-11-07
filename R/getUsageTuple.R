# getUsageTuple ----------------------------------------------------------------

#' Get Usage Tuple (Usage, Yield, Irrigation) from NUTZUNG and TYP
#'
#' @param usage value of column NUTZUNG in input data frame
#' @param type value of column TYP in input data frame
#' @param include_inputs logical indicating whether or not to include the
#'   input values in the output
#' @return list with elements \code{usage}, \code{yield}, \code{irrigation}
#' @export
#' @examples
#' getUsageTuple(10, 10)
#' getUsageTuple(10, 1:3)
getUsageTuple <- function(usage, type, include_inputs = FALSE)
{
  #usage = 10L; type = 10L
  #usage = 10L; type = 333L

  # Prepare data for which to lookup value combinations in the lookup table
  data <- data.frame(
    berlin_usage = usage,
    berlin_type = type
  )

  key_columns <- names(data)
  value_columns <- c("usage", "yield", "irrigation")

  # Provide lookup table
  lookup <- BERLIN_TYPES_TO_USAGE_YIELD_IRRIGATION

  result <- value_columns %>%
    lapply(function(value_column) {
      multi_column_lookup(
        data = data,
        lookup = lookup[, c(key_columns, value_column)],
        value = value_column
      )
    }) %>%
    stats::setNames(value_columns) %>%
    do.call(what = data.frame)

  is_missing <- is.na(result[[value_columns[1L]]])

  if (!any(is_missing)) {

    if (include_inputs) {
      result <- cbind(data, result)
    }

    return(result)
  }

  stop_formatted(
    "Could not find a (usage, yield, irrigation) tuple for %s",
    paste(collapse = ", ", sprintf(
      "(NUTZUNG = %d, TYP = %d)",
      usage[is_missing],
      type[is_missing]
    ))
  )
}
