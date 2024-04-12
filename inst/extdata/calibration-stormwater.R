# Calibration script to set stormwater-management parameters
# based on the WABILA equations
kwb.utils::assignPackageObjects("kwb.rabimo");`%>%` <- magrittr::`%>%`

# import config object
config <- kwb.rabimo::rabimo_inputs_2020$config

# Method:
#   - generate test areas
#   - model them in Wabila
#   - model them in Rabimo, compare results and assess differences

# main
if (FALSE) {

  # CASE 1) simple roof area, 100% connected to the sewer ~~~~~~~~~~~~~~~~~~~~~~
  simple_roof_area <- generate_rabimo_area(
    code = "only_roof", roof = 1, pvd = 0
  )

  wabila_roof <- calculate_wabila_roof(
    area = simple_roof_area, retention_height = 0.5
  )

  factor_simple_roof <- get_area_balance(area = simple_roof_area,
                                         config = config,
                                         wabila_results = wabila_roof)
  factor_simple_roof

  # CASE 2) green roof area: the whole roof is green ~~~~~~~~~~~~~~~~~~~~~~~~~~~
  green_roof_area <- generate_rabimo_area(
    code = "only_green_roof", roof = 1, green_roof = 1, pvd = 0)

  wabila_green_roof <- calculate_wabila_green_roof(
    area = green_roof_area, height = 40, kf = 70, w_diff = 0.5)


  factor_green_roof <- get_area_balance(area = green_roof_area,
                                        config = config,
                                        wabila_results = wabila_green_roof)

  # case 3) simple paved area, no roof, no unsealed surface ~~~~~~~~~~~~~~~~~~~~
  paved_area <- generate_rabimo_area(
    code = "all_paved", roof = 0, pvd = 1, swg_pvd = 0.38,
    srf3_pvd = 0.1, srf4_pvd = 0.2)

  wabila_paved <- calculate_wabila_paved(
    area = paved_area, joint_perc = 8, retention = 1, kf = 36, w_diff = 0.15)

  factors_paved_area <- get_area_balance(area = paved_area,
                                         config = config,
                                         wabila_results = wabila_paved)

  # CASE 4) only garden area, non constructed ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  only_garden_area <- generate_rabimo_area(code ="garden", roof = 0, pvd = 0,
                                           veg_class = 20)

  wabila_garden <- c(0.0, 0.4, 0.6)

  factors_only_garden <- get_area_balance(area = only_garden_area,
                                          config = config,
                                          wabila_results = wabila_garden)

  # case 5) mixed roof/garden area ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 50% roof and 50% garden. The runoff from the roof goes into the sewer.
  mixed_roof_garden_area <- generate_rabimo_area(code = "roof_and_garden",
                                                 roof = 0.5, pvd = 0,
                                                 veg_class = 20)

  roof_fraction <- mixed_roof_garden_area$roof
  wabila_mix_roof_garden <- wabila_garden * roof_fraction +
    wabila_roof * (1 - roof_fraction)

  factors_mixed_garden <- get_area_balance(area = mixed_roof_garden_area,
                                           config = config,
                                           wabila_results = wabila_mix_roof_garden)

  # case 6) area connected to an infiltration swale ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  area_to_swale <- generate_rabimo_area(code = "roof_to_swale",
                                        roof = 0.5, pvd = 0, to_swale = 1,
                                        veg_class = 20)
  # problem: "free" area for the swale must be taken into account
  # when comparing results with wabila simulation

  # Wabila simulation:
  # calculate first water-balance from roof area (how?)
  discharge_into_swale <- mixed_roof_garden_area$prec_yr *
    factors_mixed_garden["rabimo","runoff"]

  wabila_swale <- calculate_wabila_swale(
    area = area_to_swale, kf = 14)

  roof_fraction <- area_to_swale$roof
  swale_fraction <- attr(wabila_swale, "estimated swale area")
  garden_fraction <- 1 - roof_fraction - swale_fraction

  wabila_swale_roof_garden <- c(
    a = swale_fraction * wabila_swale[1] + garden_fraction * wabila_garden[1],
    g = roof_fraction * (wabila_roof[1] + wabila_roof[2]) + garden_fraction * wabila_garden[2] + swale_fraction * wabila_swale[2],
    v = roof_fraction * wabila_roof[3] + swale_fraction * wabila_swale[3] + garden_fraction * wabila_garden[3]
  )

  # modify config to match wabila simulation
  config$swale["swale_evaporation_factor"] <- 0.0

  factors_roof_to_swale <- get_area_balance(area = area_to_swale,
                                            config = config,
                                            wabila_results = wabila_swale_roof_garden)
}

# calculate_wabila_roof --------------------------------------------------------
calculate_wabila_roof <- function(area, retention_height)
{
  # get climatic parameters from area
  P <- area$prec_yr
  ET_p <- area$epot_yr

  # runoff component
  a <- 0.9115 + 0.00007063 * P - 0.000007498 * ET_p - 0.2063 *
    log(retention_height + 1)

  # infiltration component
  g <- 0

  #evaporation component
  v <- 1 - a

  c(a, g, v)

}

# calculate_wabila_green_roof --------------------------------------------------
calculate_wabila_green_roof <- function(area, height, kf, w_diff)
{

  # h: depth, in cm
  # kf: permeability of soil, in mm/h
  # w_diff: difference between water holding capacity and wilting point, unitless

  # get climatic parameters from area
  P <- area$prec_yr
  ET_p <- area$epot_yr

  # runoff component
  a = -2.182 + 0.4293 * log(P) - 0.0001092 * P +  236.1/ET_p +
    0.0001142 * height + 0.0002297 * kf + 0.01628 * log(w_diff) - 0.1214 *
    log((w_diff) * height)

  # infiltration component
  g <- 0

  #evaporation component
  v <- 1 - a

  c(a, g, v)
}

# calculate_wabila_paved -------------------------------------------------------
calculate_wabila_paved <- function(area, joint_perc, retention, kf, w_diff){

  # joint_perc: percentage of joints
  # retention: retention_heigth, in mm
  # kf: permeability of joints, in mm/h
  # w_diff: difference between water holding capacity and wilting point, unitless

  # get climatic parameters from area
  P <- area$prec_yr
  ET_p <- area$epot_yr

  if (2 <= joint_perc & 5.5 >= joint_perc)
  {
    a = 0.800734 * log(P) - 0.0582828 * joint_perc - 0.0501693 *
      retention - 0.385767 * (w_diff) + 8.7040284 / (11.9086896 + kf)
    g = -0.2006 - 0.000253 * ET_p + 0.05615 * joint_perc - 0.0636 *
      log(1 + retention) + 0.1596 * log(1 + kf) + 0.2778 * w_diff
    v = 0.8529 - 0.1248 * log(P) + 0.00005057 * ET_p + 0.002372 *
      joint_perc + 0.1583 * log(1 + retention)

  } else if (5.5 < joint_perc & 10 >= joint_perc)
  {
    a = 0.05912 * log(P) - 0.02749 * joint_perc - 0.03671 *
      retention - 0.30514 * (w_diff) + 4.97687 / (4.7975 + kf)
    g = 0.00004941 * P - 0.0002817 * ET_p + 0.02566 *
      joint_perc - 0.03823 * retention + 0.691 * exp(-6.465 / kf)
    v = 0.9012 - 0.1325 * log(P) + 0.00006661 * ET_p + 0.002302 * joint_perc +
      0.1489 * log(1 + retention)
  }

  c(a, g, v)
}

# calculate_wabila_swale -------------------------------------------------------
calculate_wabila_swale <- function(input = NULL, area, kf, BA = NULL)
{

  # kf: permeability, in mm/h
  # input: the discharge to the swale in mm
  # BA: percentage of swale area (what is it?)

  # get climatic parameters from area
  P <- area$prec_yr
  ET_p <- area$epot_yr

  if (is.null(input)){
    input <- P
  }

  if (is.null(BA)){
    #BA = 42.323 * kf^(-0.314)
    BA <- estimate_swale_area(kf)
  }

  g = 0.8608 + 0.02385 * log(input) - 0.00005331 * ET_p - 0.002827 * BA -
    0.000002493 * kf + 0.0009514 * log(kf/BA)
  v = 0.000008562 * ET_p + 2.611/(-64.35 + input) * BA ^ 0.9425 -
    0.000001211 * kf
  a = 1 - g - v

  result <- c(a, g, v)
  attr(result, "estimated swale area") <- BA/100

  return(result)

}

estimate_swale_area <- function(kf){

  stopifnot(kf>=14 && kf<= 3600)

  k_min <- 14
  k_max <- 3600
  A_min <- 27.14
  A_max <- 62.414
  E_min <- -0.303
  E_max <- -0.328


  A_value <- A_min + ((kf - k_min) * (A_max - A_min)) / (k_max - k_min)
  E_value <- E_min + ((kf - k_min) * (E_max - E_min)) / (k_max - k_min)

  A_value * kf ^ E_value
}



# get_area_balance -------------------------------------------------------------
get_area_balance <- function(area, config, rabimo_results = NULL,
                             wabila_results = NULL,
                             simulate_abimo = FALSE, check = FALSE)
{
  results <- if (is.null(rabimo_results)){
    run_rabimo(data = area, config = config,
               simulate_abimo = simulate_abimo,
               check = check)
  } else {
    rabimo_results
  }

  results <- results %>%
    select_columns(c("ROW","RI","VERDUNSTUN"))

  precipitation <- area[["prec_yr"]]

  factors <- results %>%
    magrittr::divide_by(precipitation) %>%
    setNames(c("runoff","infiltration","evaporation")) %>%
    as.matrix() %>%
    magrittr::set_rownames("rabimo")

  if (length(wabila_results) == 3){
    factors <- factors %>%
      rbind(wabila_results) %>%
      magrittr::set_rownames(c("rabimo", "wabila"))
  }

  return(factors)
}

