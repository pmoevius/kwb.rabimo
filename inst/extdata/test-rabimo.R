#
# Test kwb.rabimo
#
# - Source the whole script first to load the functions defined below
# - Manually go through the MAIN sections within "if (FALSE) {...}"
#

# MAIN: Provide function arguments for run_rabimo() ----------------------------
if (FALSE)
{
  # Define a path "dictionary"
  get_path <- kwb.utils::createAccessor(kwb.utils::resolve(list(
    amarex_ap4 = "Y:/SUW_Department/Projects/AMAREX/Work-packages/AP_4",
    isu5_2020 = "<amarex_ap4>/ABIMO_Daten/ISU5_2020_datengrundlage",
    data_2020 = "<isu5_2020>/isu5_2020_berlin/cleaned",
    berlin_2020 = "<data_2020>/isu5_2020_abimo_cleaned.dbf",
    ndvi = "Y:/Z-Exchange/Philipp/Amarex/NDVI R/combined_data_NDVI.dbf"
  )))

  # Read dbf file. Do not convert character to factor (as.is = TRUE)
  berlin_2020_data <- foreign::read.dbf(get_path("berlin_2020"), as.is = TRUE)

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

# MAIN: Test comparison with abimo results -------------------------------------
if (FALSE)
{
  # test with 2020 data
  # //////////////////
  input_abimo <- head(berlin_2020_data)

  # Set STR_BELAG[1-4] to 0
  for (column in c("STR_BELAG1", "STR_BELAG2", "STR_BELAG3", "STR_BELAG4")) {
    input_abimo[[column]] <- 0
  }

  # Provide configuration for kwb.abimo
  config_abimo <- kwb.abimo::read_config()

  # Run C++ Abimo using the wrapper package kwb.abimo
  kwb.abimo::run_abimo(input_data = head(input_abimo), config = config_abimo)

  # Provide configuration for kwb.rabimo
  config_rabimo <-  kwb.rabimo:::abimo_config_to_config(config_abimo)

  # Provide input data for kwb.rabimo
  input_rabimo <- kwb.rabimo::prepare_input_data(input_abimo, config_rabimo)

  # Run R-Abimo
  kwb.rabimo::run_rabimo(input = input_rabimo, config = config_rabimo)

  # test with 2019 data
  # //////////////////

  # Load Berlin data from the R-wrapper package kwb.abimo
  data <- head(kwb.abimo::abimo_input_2019)

  # Provide Abimo's default configuration
  abimo_config <-kwb.abimo::read_config()

  # Use the R-wrapper to run Abimo.exe
  abimo_result <- kwb.abimo::run_abimo(input_data = data, config = abimo_config)

  # Prepare a configuration for R-Abimo, based on the default Abimo configuration
  rabimo_config <- kwb.rabimo::abimo_config_to_config(abimo_config)
  #config_test <- config

  # prepare data for rabimo format
  rabimo_data <- kwb.rabimo::prepare_input_data(data, rabimo_config)

  # Run R-Abimo, the R-implementation of Abimo in this package
  rabimo_result <- kwb.rabimo::run_rabimo(rabimo_data, rabimo_config)

  # Have a look at the first lines of the result data frames
  abimo_result
  rabimo_result


  # fictive areas for testing
  # /////////////////////////

  data <- head(kwb.abimo::abimo_input_2019)
  btf1 <- data[3,]
  btf1$CODE <- "mixed-surfaces"
  btf1$FLGES <- 100
  btf1$PROBAU <- 40
  btf1$PROVGU <- 20
  btf1$STR_FLGES <- 0
  btf1$VGSTRASSE <- 0
  btf1$VG <- btf1$PROBAU + btf1$PROVGU
  btf1[, c("STR_BELAG1", "STR_BELAG2", "STR_BELAG3", "STR_BELAG4")] <- 0
  btf1[, c("BELAG2", "BELAG3", "BELAG4")] <- 0L
  btf1$BELAG1 <- 100L
  btf1$KAN_VGU <- 60L
  btf1$KAN_STR <- 100L

  btf2 <- btf1
  btf2$CODE <- "no-green"
  btf2$FLGES <- 100
  btf2$PROBAU <- 40
  btf2$PROVGU <- 60
  btf2$STR_FLGES <- 0
  btf2$VGSTRASSE <- 0
  btf2$VG <- btf2$PROBAU + btf2$PROVGU
  btf2[, c("STR_BELAG1", "STR_BELAG2", "STR_BELAG3", "STR_BELAG4")] <- 0
  btf2[, c("BELAG2", "BELAG3", "BELAG4")] <- 0L
  btf2$BELAG1 <- 100L
  btf2$KAN_VGU <- 60L
  btf2$KAN_STR <- 100L

  btf3 <- btf2
  btf3$CODE <- "only-roof"
  btf3$FLGES <- 100
  btf3$PROBAU <- 100
  btf3$PROVGU <- 0
  btf3$STR_FLGES <- 0
  btf3$VGSTRASSE <- 0
  btf3$VG <- btf3$PROBAU + btf3$PROVGU
  btf3[, c("STR_BELAG1", "STR_BELAG2", "STR_BELAG3", "STR_BELAG4")] <- 0
  btf3[, c("BELAG2", "BELAG3", "BELAG4")] <- 0L
  btf3$BELAG1 <- 100L
  btf3$KAN_VGU <- 60L
  btf3$KAN_STR <- 100L

  btf4 <- btf2
  btf4$CODE <- "only-paved"
  btf4$FLGES <- 100
  btf4$PROBAU <- 0
  btf4$PROVGU <- 100
  btf4$STR_FLGES <- 0
  btf4$VGSTRASSE <- 0
  btf4$VG <- btf4$PROBAU + btf4$PROVGU
  btf4[, c("STR_BELAG1", "STR_BELAG2", "STR_BELAG3", "STR_BELAG4")] <- 0
  btf4[, c("BELAG2", "BELAG3", "BELAG4")] <- 0L
  btf4$BELAG1 <- 100L
  btf4$KAN_VGU <- 60L
  btf4$KAN_STR <- 100L

  btf5 <- btf2
  btf5$CODE <- "only-green"
  btf5$FLGES <- 100
  btf5$PROBAU <- 0
  btf5$PROVGU <- 0
  btf5$STR_FLGES <- 0
  btf5$VGSTRASSE <- 0
  btf5$VG <- btf5$PROBAU + btf5$PROVGU
  btf5[, c("STR_BELAG1", "STR_BELAG2", "STR_BELAG3", "STR_BELAG4")] <- 0
  btf5[, c("BELAG2", "BELAG3", "BELAG4")] <- 0L
  btf5$BELAG1 <- 100L
  btf5$KAN_VGU <- 60L
  btf5$KAN_STR <- 100L

  btf6 <- btf1
  btf6$CODE <- "mixed-w-roads"
  btf6$FLGES <- 100
  btf6$PROBAU <- 40
  btf6$PROVGU <- 20
  btf6$STR_FLGES <- 10
  btf6$VGSTRASSE <- 0
  btf6$VG <- btf6$PROBAU + btf6$PROVGU
  btf6[, c("STR_BELAG1", "STR_BELAG2", "STR_BELAG3", "STR_BELAG4")] <- 0
  btf6[, c("BELAG2", "BELAG3", "BELAG4")] <- 0L
  btf6$BELAG1 <- 100L
  btf6$KAN_VGU <- 60L
  btf6$KAN_STR <- 100L

  data <- rbind(btf1,btf2,btf3,btf4,btf5,btf6)
  data$TYP <- 10L
  data$NUTZUNG <- 10L

  # Provide Abimo's default configuration
  abimo_config <-kwb.abimo::read_config()

  # Use the R-wrapper to run Abimo.exe
  abimo_result <- kwb.abimo::run_abimo(input_data = data, config = abimo_config)

  # Prepare a configuration for R-Abimo, based on the default Abimo configuration
  config <- kwb.rabimo::abimo_config_to_config(abimo_config)

  # prepare data for rabimo format
  data <- kwb.rabimo::prepare_input_data(data, config)

  # Run R-Abimo, the R-implementation of Abimo in this package
  rabimo_result <- kwb.rabimo::run_rabimo(data, config)

  # Have a look at the first lines of the result data frames
  abimo_result
  rabimo_result

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
