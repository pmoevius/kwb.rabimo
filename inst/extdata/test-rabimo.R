#
# Test kwb.rabimo
#
# - Source the whole script first to load the functions defined below!
#

if (FALSE)
{
  # Define a path "dictionary"
  get_path <- kwb.utils::createAccessor(kwb.utils::resolve(list(
    amarex_ap4 = "//medusaY:/SUW_Department/Projects/AMAREX/Work-packages/AP_4",
    isu5_2020 = "<amarex_ap4>/ABIMO_Daten/ISU5_2020_datengrundlage",
    data_2020 = "<isu5_2020>/isu5_2020_berlin/cleaned",
    berlin_2020 = "<data_2020>/isu5_2020_abimo_cleaned.dbf",
    ndvi = "Y:/Z-Exchange/Philipp/Amarex/NDVI R/combined_data_NDVI.dbf"
  )))

  berlin_2020_data <- foreign::read.dbf(get_path("berlin_2020"))

  # PROBLEM: HANDLE NAs!!

  # Clean column "STR_FLGES"
  table_with_na(berlin_2020_data$STR_FLGES)
  berlin_2020_data$STR_FLGES <- 0

  #berlin_2019_data <- kwb.abimo::abimo_input_2019

  # Provide Abimo default configuration
  config <- kwb.rabimo::abimo_config_to_config(kwb.abimo::read_config())

  # Convert input data to the new format
  input_backup <- kwb.rabimo::prepare_input_data(berlin_2020_data, config)

  # Read NDVI data
  ndvi_matrix <- foreign::read.dbf(get_path("ndvi"))

  # Merge the NDVI value to the input data (by the codes of the blocks)
  input_ndvi <- dplyr::left_join(
    input_backup,
    kwb.utils::selectColumns(ndvi_matrix, c("code", "ndvi_value")),
    by = "code"
  )

  # Do we have an NDVI value for each block?
  table_with_na(input_ndvi$ndvi_value)

  #  30    35    40    45    50  <NA>
  #   1  1583 24192 29384  3371     0

  # Save the original vegetation class (as assigned by hard-coded rules in
  # original ABIMO source code)
  old_veg_class <- input_ndvi$veg_class

  # Use the NDVI value as vegetation class
  input_ndvi[["veg_class"]] <- input_ndvi[["ndvi_value"]]

  # Remove the NDVI column
  input_ndvi[["ndvi_value"]] <- NULL

  # Remove rows where groundwater distance is NA
  input <- input_ndvi[has_gw_dist(input_ndvi), ]

  # use for old vegetation classes
  input <- input_backup[has_gw_dist(input_backup), ]
  input <- head(input)
}

# Define function: table_with_na() ---------------------------------------------
table_with_na <- function(x)
{
  table(x, useNA = "always")
}

# Define function: has_gw_dist() -----------------------------------------------
has_gw_dist <- function(data)
{
  kwb.utils::matchesCriteria(data, "!is.na(gw_dist)")
}
