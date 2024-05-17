# get_expected_data_types ------------------------------------------------------
get_expected_data_types <- function()
{
  columns_to_named_vector(
    data = read_column_info(),
    key_column = "rabimo_berlin",
    value_column = "data_type"
  )
}
