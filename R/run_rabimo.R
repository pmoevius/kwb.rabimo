# run_rabimo -------------------------------------------------------------------

#' Run R-Abimo, the R-implementation of Water Balance Model Abimo
#'
#' @details If \code{input_data} contains a column \code{yield} it is expected that
#'   the data are already prepared as otherwise would do the function
#'   \code{\link{prepare_input_data}}.
#'
#' @param input data frame with columns as required by Abimo or
#'   data frame with columns as returned by \code{\link{prepare_input_data}}
#' @param config configuration object (list) as returned by the function
#'   \code{abimo_config_to_config()} used on \code{kwb.abimo:::read_config()}
#' @param simulate_abimo logical of length one indicating whether or not to
#'   simulate exactly what Abimo does (including obvious errors!).
#'   Default: \code{TRUE}!
#' @return data frame with columns as returned by Abimo
#' @export
run_rabimo <- function(input, config, simulate_abimo = TRUE)
{

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
    #berlin_2019_data <- kwb.abimo::abimo_input_2019
    config <- abimo_config_to_config(kwb.abimo:::read_config())
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
  }


  # check whether the input data have the expected structure
  if (!"code" %in% names(input)) {
    stop(
      "input data has not the expected format.",
      "I was looking for column 'code'",
      "You might want to use the function prepare_berlin_data().", call. = FALSE
    ) # currently called prepare_input_data
  }

  # Create accessor functions to input columns and config elements
  fetch_input <- create_accessor(input)
  fetch_config <- create_accessor(config)
  #get_fraction <- create_fraction_accessor(input)

  # Get climate data
  climate <- cat_and_run(
    "Collecting climate related data from input",
    get_climate(input)
  )

  # Prepare soil properties for all rows. They are required to calculate the
  # actual evapotranspiration of unsealed areas. In the case of water bodies,
  # all values are 0.0. (hsonne: really?)
  soil_properties <- cat_and_run(
    "Preparing soil property data for all block areas",
    expr = get_soil_properties(
      land_type = fetch_input("land_type"),
      veg_class = fetch_input("veg_class"),
      depth_to_water_table = fetch_input("gw_dist"),
      field_capacity_30 = fetch_input("ufc30"),
      field_capacity_150 = fetch_input("ufc150"),
      dbg = FALSE
    )
  )

  stopifnot(!anyNA(soil_properties$mean_potential_capillary_rise_rate))

  # Precalculate all results of realEvapoTranspiration()
  evaporation_sealed <- cat_and_run(
    "Precalculating actual evapotranspirations for impervious areas",
    expr = fetch_config("bagrov_values") %>%
      lapply(function(x) {
        real_evapo_transpiration(
          potential_evaporation = select_columns(climate, "epot_yr"),
          x_ratio = select_columns(climate, "x_ratio"),
          bagrov_parameter = rep(x, nrow(input)),
          use_abimo_algorithm = simulate_abimo
        )
      }) %>%
      do.call(what = data.frame)
  )

  # Precalculate all results of actualEvaporationWaterbodyOrPervious()
  evaporation_unsealed <- cat_and_run(
    paste(
      "Precalculating actual evapotranspirations for waterbodies or pervious",
      "areas"
    ),
    actual_evaporation_waterbody_or_pervious(
      usage_tuple = fetch_input(c("land_type", "veg_class", "irrigation")),
      climate = climate,
      soil_properties = soil_properties,
      min_size_for_parallel = 100L,
      #digits = 3L,
      use_abimo_algorithm = simulate_abimo
    )
  )

  runoff_all <- climate[["prec_yr"]] - cbind(
    evaporation_sealed,
    unsealed = evaporation_unsealed
  )

  # Runoff for all sealed areas (including roofs)

  # Calculate roof related variables

  # total runoff of roof areas
  # (total runoff, contains both surface runoff and infiltration components)
  runoff_roof <- select_columns(runoff_all, "roof")

  # Provide runoff coefficients for impervious surfaces
  runoff_factors <- fetch_config("runoff_factors")

  # actual runoff from roof surface (area based, with no infiltration)
  runoff_roof_actual <- with(input, swg_roof) *
    runoff_factors[["roof"]] * runoff_roof

  # actual infiltration from roof surface (area based, with no runoff)
  infiltration_roof_actual <- with(input, roof * (1-swg_roof)) *
    runoff_roof

  # Calculate runoff for all surface classes at once
  # (contains both surface runoff and infiltration components)

  # choose columns related to surface classes
  runoff_sealed <- filter_elements(runoff_all, "surface")
  # head(runoff_sealed)

  # Runoff from the actual partial areas that are sealed and connected
  # (road and non-road) areas (for all surface classes at once)

  runoff_factor_matrix <- expand_to_matrix(
    x = filter_elements(runoff_factors, "surface"),
    nrow = nrow(input)
  )

  unbuilt_surface_fractions <- fetch_input(paste0("srf", 1:4,"_pvd"))
  road_surface_fractions <- fetch_input(paste0("srf", 1:4,"_pvd_rd"))


  #ab hier weiter aktualisieren
  runoff_sealed_actual <- runoff_factor_matrix * (
    get_fraction("main/unbuiltSealed/connected") * unbuilt_surface_fractions +
      get_fraction("road/roadSealed/connected") * road_surface_fractions
  ) *
    runoff_sealed

  runoff_sealed_actual <- runoff_factor_matrix * (
    get_fraction() * unbuilt_surface_fractions +
      get_fraction("road/roadSealed/connected") * road_surface_fractions
  ) *
    runoff_sealed

  # infiltration of sealed surfaces (for all surface classes at once)
  infiltration_sealed_actual <- (
    get_fraction("main/unbuiltSealed") * unbuilt_surface_fractions +
      get_fraction("road/roadSealed") * road_surface_fractions
  ) *
    runoff_sealed -
    runoff_sealed_actual

  # Surface-Runoff of unsealed surfaces (unsealedSurface_RUV)
  runoff_unsealed <- prec_year - evaporation_unsealed

  # Infiltration of road (unsealed areas)
  infiltration_unsealed_roads <-
    get_fraction("road/!roadSealed") *
    # last surface class
    runoff_sealed[, ncol(runoff_sealed)]

  # Infiltration from unsealed non-road surfaces (old: riuv)

  fraction_unsealed <- if (simulate_abimo) {
    #fetch("areaFractionMain") * # ??? TODO: VERIFY THIS ????
    (1 - fetch_input("mainFractionSealed"))
  } else {
    get_fraction("main/!sealed")
  }

  # original C++ code (check if correct):
  # infiltration.unsealedSurfaces = (
  #   100.0F - current_area.mainPercentageSealed()
  # ) / 100.0F * runoff.unsealedSurface_RUV;

  infiltration_unsealed_surfaces <- fraction_unsealed * runoff_unsealed

  # Calculate infiltration rate 'RI' for entire block partial area (mm/a)

  total_infiltration <-
    infiltration_roof_actual +
    infiltration_unsealed_surfaces +
    infiltration_unsealed_roads +
    rowSums(infiltration_sealed_actual)

  # Calculate runoff 'ROW' for entire block area (FLGES + STR_FLGES) (mm/a)

  total_surface_runoff <- runoff_roof_actual +
    #orig.: runoff_unsealed_roads <- was set to zero in the master branch
    rowSums(runoff_sealed_actual)

  # Calculate "total system losses" 'R' due to runoff and infiltration
  # for entire block partial area
  total_runoff <- total_surface_runoff + total_infiltration

  # Calculate evaporation 'VERDUNST' by subtracting 'R', the sum of
  # runoff and infiltration from precipitation of entire year,
  # multiplied by precipitation correction factor
  total_evaporation <- prec_year - total_runoff

  # Provide total area for calculation of "flows"
  total_area <- fetch_input("totalArea")

  # Calculate volume 'rowvol' from runoff (qcm/s)
  surface_runoff_flow <- yearly_height_to_volume_flow(
    total_surface_runoff, total_area
  )

  # Calculate volume 'rivol' from infiltration rate (qcm/s)
  infiltration_flow <- yearly_height_to_volume_flow(
    total_infiltration, total_area
  )

  # Calculate volume of "system losses" 'rvol' due to surface runoff and
  # infiltration
  total_runoff_flow <- surface_runoff_flow + infiltration_flow

  # Provide mapping between local variable names and ABIMO-output columns
  name_mapping <- list(
    code = "CODE",
    total_runoff = "R",
    total_surface_runoff = "ROW",
    total_infiltration = "RI",
    total_runoff_flow = "RVOL",
    surface_runoff_flow = "ROWVOL",
    infiltration_flow = "RIVOL",
    total_area = "FLAECHE",
    total_evaporation = "VERDUNSTUN"
  )

  # Compose result data frame. Use mget() to get the result vectors from the
  # local environment and put them into the data frame
  result_data <- cbind(
    fetch_input("code", drop = FALSE),
    mget(names(name_mapping)[-1L])
  )

  # Provide the same columns as Abimo does
  abimo_result <- rename_and_select(result_data, name_mapping)

  # Round all columns to three digits (skip first column: "CODE")
  abimo_result[-1L] <- lapply(abimo_result[-1L], round, 3L)

  # Return intermediate results as attributes
  structure(
    abimo_result,
    input = input,
    result_data = result_data,
    intermediates = list(
      climate = climate,
      soil_properties = soil_properties,
      evaporation_sealed = evaporation_sealed,
      evaporation_unsealed = evaporation_unsealed,
      roof = list(
        evaporation_roof = evaporation_sealed[["roof"]],
        runoff_roof = runoff_roof,
        runoff_roof_actual = runoff_roof_actual,
        infiltration_roof_actual = infiltration_roof_actual
      ),
      surface = list(
        evaporation_sealed = evaporation_sealed[, -1L],
        runoff_sealed = runoff_sealed,
        runoff_sealed_actual = runoff_sealed_actual,
        infiltration_sealed_actual = infiltration_sealed_actual
      ),
      runoff = cbind(
        unsealedSurface = runoff_unsealed,
        unsealedRoads = runoff_sealed[, ncol(runoff_sealed)],
        sealed = runoff_sealed
      ),
      fraction_unsealed = fraction_unsealed
    )
  )
}
