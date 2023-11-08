# create_fraction_accessor -----------------------------------------------------
create_fraction_accessor <- function(data)
{
  function(path) {
    columns <- fraction_path_to_names(path)
    negated <- get_attribute(columns, "negated")
    data <- select_columns(data, columns, drop = FALSE)
    data[negated] <- lapply(data[negated], function(x) 1 - x)
    Reduce(`*`, data)
  }
}

# fraction_path_to_names -------------------------------------------------------
fraction_path_to_names <- function(path)
{
  parts <- strsplit(path, "/")[[1L]]

  # Which columns are to be "negated", i.e. 1 - data[[column]] will be used
  pattern <- "^[!]"
  negated <- grepl(pattern, parts)

  # Remove the exclamation mark
  parts[negated] <- gsub(pattern, "", parts[negated])

  structure(
    paste0(c("area", parts[-length(parts)]), "Fraction", first_upper(parts)),
    negated = negated
  )
}
