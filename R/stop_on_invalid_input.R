# stop_on_invalid_input --------------------------------------------------------
stop_on_invalid_input <- function(input)
{
  if (!"code" %in% names(input)) {
    clean_stop(
      "input data has not the expected format. ",
      "I was looking for column 'code'",
      "You might want to use the function prepare_berlin_data()."
    )
  }

  # Stop if any required column is missing
  column_info <- read_column_info()
  required <- column_info$rabimo_berlin[column_info$type == "required"]
  available <- names(input)
  missing <- setdiff(required, available)

  if (length(missing)) {
    column_info[column_info$rabimo_berlin %in% missing, ] %>%
      rename_and_select(list(rabimo_berlin = "column", "meaning", "unit")) %>%
      reset_row_names() %>%
      print_if(condition = TRUE, caption = "Missing columns")
    clean_stop(
      "There are missing columns (see above). ",
      "Please make sure that you provide all required columns."
    )
  }

  # Stop if a column does not have the expected data type
  check_data_types(input, types = get_expected_data_type(names(input)))

  # Do not accept any NA
  check_columns(
    data = input,
    columns = intersect(
      column_info$rabimo_berlin[column_info$data_type == "numeric"],
      names(input)
    ),
    check = function(x) !is.na(x),
    msg = paste(
      "Column '%s' must not contain missing values (NA, found %d times).",
      "Please give a value (may be 0) in each row."
    )
  )

  # Check precipitation and evapotranspiration for negative values
  check_columns(
    data = input,
    columns = c("prec_yr", "prec_s", "epot_yr", "epot_s"),
    check = function(x) x >= 0,
    msg = paste(
      "There are negative values in column '%s' (%d-times).",
      "Please make sure that all values are greater than or equal to zero."
    )
  )

  # Check fractions
  check_columns(
    data = input,
    columns = column_info$rabimo_berlin[column_info$unit == "0..1"],
    check = function(x) kwb.utils::inRange(x, 0, 1),
    msg = paste(
      "Not all values in column '%s' are between 0 and 1 as expected",
      "(%d failures)."
    )
  )

  surface_columns <- sprintf("srf%d_pvd", 1:4)
  surface_columns_rd <- paste0(surface_columns, "_rd")

  check_sum_up_to_1_or_0(input, columns = surface_columns)
  check_sum_up_to_1_or_0(input, columns = surface_columns_rd)
}

# get_expected_data_type -------------------------------------------------------
get_expected_data_type <- function(x)
{
  info <- read_column_info() %>%
    dplyr::filter(.data[["rabimo_berlin"]] %in% x) %>%
    create_accessor()

  stats::setNames(info("data_type"), info("rabimo_berlin"))
}

# check_data_types -------------------------------------------------------------
check_data_types <- function(input, types)
{
  for (column in names(types)) {
    data_type <- mode(select_columns(input, column))
    if (data_type != types[column]) {
      stop_formatted(
        "Column '%s' (%s) does not have the expected data type (%s).",
        column, data_type, types[column]
      )
    }
  }
}

# check_sum_up_to_1_or_0 -------------------------------------------------------
check_sum_up_to_1_or_0 <- function(input, columns, tolerance = 0.005)
{
  x <- select_columns(input, columns)

  row_sums <- rowSums(x)

  differs <- function(a, b) abs(a - b) > tolerance

  is_invalid <- differs(row_sums, 0) & differs(row_sums, 1)

  if (any(is_invalid)) {
    cat("(First) invalid rows:\n")
    print(utils::head(select_columns(input, c("code", columns))[is_invalid, ]))
    stop_formatted(
      paste(
        "The sum of columns %s is not 1 or 0 in each row as expected",
        "(see above). The tolerance was: %f"
      ),
      kwb.utils::stringList(columns),
      tolerance
    )
  }
}
