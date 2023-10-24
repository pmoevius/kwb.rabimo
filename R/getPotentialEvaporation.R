# getPotentialEvaporation ------------------------------------------------------

#' Provide Data on Potential Evaporation
#'
#' @param input data frame with columns \code{usage}, \code{district}
#' @param config list with entry \code{potentialEvaporation}
#' @param default_etp default value for etp
#' @param default_etps default value for etps
#' @param default_etp_waterbody default value for etp for usage = 110
#'   (waterbody)
#' @export
#' @examples
#' getPotentialEvaporation(data.frame(usage = 10, district = 1), config = list(
#'  potentialEvaporation = list(district_1 = list(etp = 100, etps = 200))
#' ))
#'
getPotentialEvaporation <- function(
    input,
    config,
    default_etp = 660L,
    default_etps = 530L,
    default_etp_waterbody = 775L
)
{
  #kwb.utils::assignPackageObjects("kwb.rabimo")
  #input <- kwb.abimo::abimo_input_2019[1:10, ]
  #config <- getDefaultConfiguration(1)

  select_elements <- kwb.utils::selectElements
  select_columns <- kwb.utils::selectColumns

  # If more than one row is given, call this function for each row
  if (nrow(input) > 1L) {

    results <- lapply(seq_len(nrow(input)), function(i) {
      as.data.frame(getPotentialEvaporation(input[i, ], config))
    })

    return(do.call(rbind, results))
  }

  # waterbody?
  result <- if (select_elements(input, "usage") == 110) {

    list(
      perYearInteger = default_etp_waterbody,
      inSummerInteger = -1 # check that
    )

  } else {

    element <- paste0("district_", select_columns(input, "district"))
    x <- select_elements(config, "potentialEvaporation")[[element]]
    given <- !is.null(x)

    list(
      perYearInteger = if (given) select_elements(x, "etp") else default_etp,
      inSummerInteger = if (given) select_elements(x, "etps") else default_etps
    )
  }

  result[["perYearFloat"]] = as.double(result[["perYearInteger"]])

  result
}
