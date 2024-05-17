# Functions about the natural scenarios and the calculation of Delta-W


# data_to_natural --------------------------------------------------------------

#' Transform R-Abimo input Data into their natural scenario equivalent
#'
#' Three scenarios are possible:
#' 1) undeveloped: all paved or constructed areas are set to 0%. No connection
#'   to the sewer.
#' 2) forested: like undeveloped, but the land type is declared to be
#'  "forested".
#' 3) horticultural: like undeveloped, but the land type is declared to be
#'   "horticultural".
#'
#' @param data the input data in R-Abimo format
#' @param type a character object containing the name of natural scenario.
#'   Defaults to "undeveloped"
#' @return a dataframe with R-Abimo input data for the chosen natural scenario
#' @export
data_to_natural <- function(data, type = "undeveloped")
{
  #kwb.utils::assignPackageObjects("kwb.rabimo")

  # Check if data has R-Abimo format
  stop_on_invalid_data(data)

  # define patterns for column names related to urbanisation
  patterns <- c("pv", "swg", "roof", "sealed")
  urban_columns <- grep(paste(patterns, collapse = "|"), names(data), value = TRUE)

  # non urbanized state: no building, no pavements
  nat_data <- data %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(urban_columns), ~ 0))

  if(type == "undeveloped"){
    return(nat_data)
  }

  nat_data[["land_type"]] <- if (type == "forested"){
    "forested"
  } else if (type == "horticultural"){
    "horticultural"
  } else {
    stop("please provide a known natural scenario type: undeveloped, horticultural or forested")
  }

  # Convert data types as required
  check_or_convert_data_types(
    nat_data,
    types = get_expected_data_type(),
    convert = TRUE
  )
}

# calculate_delta_W ------------------------------------------------------------

#' Deviation from Natural Water Balance (Delta-W)
#'
#' Calculate the deviation from the natural water balance (delta-W) given
#'  R-Abimo results as returned by \code{\link{run_rabimo}}.
#'
#' @param natural R-Abimo results for the natural scenario
#' @param urban R-Abimo results for the "urban" scenario
#' @param cols_to_omit column names that not contain result data or code identifiers. Defaults to "total_area"
#' @param return_codes a logical value determining whether the codes should be returned along the delta-w values
#' @return a dataframe containing the delta-w values (and optionally the areas' codes)
#' @export
calculate_delta_W <- function(natural, urban,
                              cols_to_omit = c("area"),
                              return_codes = FALSE)
{

  stopifnot("code" %in% names(natural))
  stopifnot("code" %in% names(urban))
  stopifnot(all(urban[["code"]] %in% natural[["code"]]))

  combined <- dplyr::left_join(urban,
                               kwb.utils::removeColumns(natural, cols_to_omit),
                               by = "code", suffix = c("_u","_n"))

  # variable columns
  pattern <- "_[un]$"
  column_names <- grep(pattern, names(combined), value = TRUE)
  unique_names <- unique(sub(pattern, "", column_names))

  precipitation <- rowSums(combined[,column_names])/2

  diff_list <- list()

  for (name in unique_names) {
    urban_name <- paste0(name, "_u")
    natural_name <- paste0(name, "_n")

    diff_list[[name]] <- abs(combined[[urban_name]] - combined[[natural_name]])
  }

  delta_w <- round(rowSums(as.data.frame(diff_list))/2*100/precipitation,1)

  if(return_codes){
    data.frame(code = urban[["code"]], delta_w = delta_w)
  } else {
    data.frame(delta_w = delta_w)
  }
}

calculate_delta_W_2 <- function(natural,
                                urban,
                                water_balance_vars = c("surface_runoff",
                                                       "infiltration",
                                                       "evaporation"),
                                code_column_name = "code"
                                )
{

  stopifnot(code_column_name %in% names(natural))
  stopifnot(code_column_name %in% names(urban))
  stopifnot(all(urban[[code_column_name]] %in% natural[[code_column_name]]))

  urban_codes <- urban[[code_column_name]]

  natural_selection <- natural %>%
    dplyr::filter(.data[["code"]] %in% urban_codes) %>%
    dplyr::arrange(match(.data[["code"]], urban_codes)) %>%
    `rownames<-`(NULL)

  diff_matrix <- abs(
    natural_selection[water_balance_vars] - urban[water_balance_vars])

  precipitation <- rowSums(natural_selection[water_balance_vars])

  cbind(
    code = urban_codes,
    data.frame(delta_w = round(rowSums(diff_matrix)*100/precipitation/2, 1)))

}

