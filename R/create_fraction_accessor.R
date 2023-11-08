# create_fraction_accessor -----------------------------------------------------
create_fraction_accessor <- function(data)
{
  function(path) {
    columns <- fraction_path_to_names(path)
    Reduce(`*`, select_columns(data, columns, drop = FALSE))
  }
}

# fraction_path_to_names -------------------------------------------------------
fraction_path_to_names <- function(path)
{
  parts <- strsplit(path, "/")[[1L]]
  paste0(c("area", parts[-length(parts)]), "Fraction", first_upper(parts))
}
