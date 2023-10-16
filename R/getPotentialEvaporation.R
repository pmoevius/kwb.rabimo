# getPotentialEvaporation ------------------------------------------------------

#' Provide Data on Potential Evaporation
#'
#' @param input data frame with columns \code{usage}, \code{district}
#' @param config list with entry \code{potentialEvaporation}
#' @export
#' @examples
#' getPotentialEvaporation(data.frame(usage = 10, district = 1), config = list(
#'  potentialEvaporation = list(district_1 = list(etp = 100, etps = 200))
#' ))
#'
getPotentialEvaporation <- function(input, config)
{
  result <- list()

  # waterbody
  if (input$usage == 110) {
    result$perYearInteger <- 775L
    result$inSummerInteger <- -1 # check that
  }

  district_string <- paste0("district_", input$district)

  config_element <- config$potentialEvaporation[[district_string]]

  if (!is.null(config_element)){
    result$perYearInteger <- kwb.utils::selectElements(config_element, "etp")
    result$inSummerInteger <- kwb.utils::selectElements(config_element, "etps")
  } else {
    # default values are assumed
    result$perYearInteger <- 660L
    result$inSummerInteger <- 530L
  }

  # no more correction with 1.1
  result$perYearFloat <- as.double(result$perYearInteger)

  result
}
