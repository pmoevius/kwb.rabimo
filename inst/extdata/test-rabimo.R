
# parameter for testing, to delete later
if(FALSE)
{
  kwb.utils::assignPackageObjects("kwb.rabimo");simulate_abimo = TRUE

  #paths
  path_amarex_ap4 <- "Y:/SUW_Department/Projects/AMAREX/Work-packages/AP_4"
  path_data_2020 <- file.path(
    path_amarex_ap4,
    "ABIMO_Daten/ISU5_2020_datengrundlage/isu5_2020_berlin/cleaned"
  )

  file_berlin_2020 <- file.path(path_data_2020, "isu5_2020_abimo_cleaned.dbf")

  berlin_2020_data <- foreign::read.dbf(file_berlin_2020) #PROBLEM: HANDLE NAs!!
  berlin_2020_data$STR_FLGES <- 0
  #berlin_2019_data <- kwb.abimo::abimo_input_2019
  config <- abimo_config_to_config(kwb.abimo::read_config())
  input_backup <- prepare_input_data(berlin_2020_data, config)

  ndvi_matrix <- foreign::read.dbf("Y:/Z-Exchange/Philipp/Amarex/NDVI R/combined_data_NDVI.dbf")
  input_ndvi <- dplyr::left_join(input_backup,
                                 kwb.utils::selectColumns(ndvi_matrix,
                                                          c("code", "ndvi_value")
                                 ),
                                 by = "code"
  )
  old_veg_class <- input_ndvi$veg_class
  input_ndvi[["veg_class"]] <- input_ndvi[["ndvi_value"]]
  input_ndvi[["ndvi_value"]] <- NULL

  # Remove rows where groundwater distance is NA
  input <- input_ndvi[kwb.utils::matchesCriteria(input_ndvi, "!is.na(gw_dist)"), ]

  # use for old vegetation classes
  input <- input_backup[kwb.utils::matchesCriteria(input_backup, "!is.na(gw_dist)"), ]
  input <- head(input)
}
