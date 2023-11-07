# getPotentialEvaporation -----------------------------------------------------

#' Provide Data on Potential Evaporation
#'
#' @param isWaterbody (vector of) logical indicating whether a block area is
#'   of type (from the type/yield/irrigation tuple) "waterbody"
#' @param district (vector of) integer indicating the district number of the
#'   plot area (from the original input column "BEZIRK")
#' @param lookup data frame with key columns \code{isWaterbody}, \code{district}
#'   and value columns \code{etp}, \code{etps}. A data frame of the required
#'   structure is returned by \code{\link{abimo_config_to_config}} in list
#'   element \code{"potentialEvaporation"}
#' @export
#' @examples
#' \dontrun{
#' config <- abimo_config_to_config(kwb.abimo:::read_config())
#' getPotentialEvaporation(
#'   is_waterbody = TRUE,
#'   district = 1,
#'   lookup = config$potentialEvaporation
#' )
#' }
#'
getPotentialEvaporation <- function(isWaterbody, district, lookup)
{
  # Prepare input data for multi_column_lookup
  data <- data.frame(
    isWaterbody = isWaterbody,
    district = district
  )

  # One after another, lookup values for "etp", "etps" in the lookup table
  result <- c(perYear = "etp", inSummer = "etps") %>%
    lapply(function(column) {
      multi_column_lookup(
        data = data,
        lookup = select_columns(lookup, c(names(data), column))
      )
    })

  if (all(lengths(result) == 1L)) {
    return(result)
  }

  as.data.frame(result)
}
