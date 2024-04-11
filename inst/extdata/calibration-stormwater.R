# Calibration script to set stormwater-management parameters
# based on the WABILA equations
kwb.utils::assignPackageObjects("kwb.rabimo")

# import config object
config <- kwb.rabimo::rabimo_inputs_2020$config

# modify config to match wabila simulation
config$bagrov_values["green_roof"] <- 0.5
config$swale["swale_evaporation_factor"] <- 0.0


# generate test areas -------------------------------------------------------
simple_roof_area <- generate_rabimo_area(code = "only_roof",
                                         roof = 1, pvd = 0)

green_roof_area <- generate_rabimo_area(code = "only_green_roof",
                                        roof = 1, green_roof = 1, pvd = 0)

paved_area <- generate_rabimo_area(code = "all_paved", roof = 0, pvd = 1,
                                   swg_pvd = 0.38, srf3_pvd = 0.1, srf4_pvd = 0.2)

area_to_swale <- generate_rabimo_area(code = "roof_to_swale",
                                      roof = 1, pvd = 0, to_swale = 1)

# model areas in WABILA --------------------------------------------------------

# get climatic parameters
#
P <- simple_roof_area$prec_yr
ET_p <- simple_roof_area$epot_yr

## "normal" roof equation ###
Sp <- 0.25 # retention, in  mm

a_roof <- 0.9115 + 0.00007063 * - 0.000007498 * ET_p - 0.2063 * log(Sp + 1)
g_roof <- 0
v_roof <- 1 - a_roof

wabila_roof_results <- c(a_roof, g_roof, v_roof)

## green-roof equation ###
h <- 40 # depth, in cm
k_f <- 70 # permeability of joints, in mm/h
w <- 0.5 # difference between water holding capacity and wilting point, unitless

a_green_roof = -2.182 + 0.4293 * log(P) - 0.0001092 * P +  236.1/ET_p +
  0.0001142 * h + 0.0002297 * k_f + 0.01628 * log(w) - 0.1214 * log((w) * h)
g_green_roof <- 0
v_green_roof <- 1 - a_green_roof

wabila_green_roof_results <- c(a_green_roof, g_green_roof, v_green_roof)

## equation for semi-permeable surfaces (6% to 10% joints) ###
FA = 8 # percentage of joints
Sp <- 1 # retention, in  mm
k_f <- 36 # permeability, in mm/h
w <- 0.15 # difference between water holding capacity and wilting point, unitless

a_pavement = 0.05912 * log(P) - 0.02749 * FA - 0.03671 * Sp - 0.30514 * (w) +
  4.97687 / (4.7975 + k_f)
g_pavement = 0.00004941 * P - 0.0002817 * ET_p + 0.02566 * FA - 0.03823 * Sp +
  0.691 * exp(-6.465/k_f)
v_pavement = 0.9012 - 0.1325 * log(P) + 0.00006661 * ET_p + 0.002302 * FA +
  0.1489 * log(1 + Sp)

wabila_paved_results <- c(a_pavement, g_pavement, v_pavement)

## equation for infiltration swale ###
k_f = 14 # permeability, in mm/h
BA = 42.323 * k_f^(-0.314) # percentage of swale area (what is it?)

g_swale = 0.8608 + 0.02385 * log(P) - 0.00005331 * ET_p - 0.002827 * BA -
  0.000002493 * k_f + 0.0009514 * log(k_f/BA)
v_swale = 0.000008562 * ET_p + 2.611/(-64.35 + P) * BA ^ 0.9425 -
  0.000001211 * k_f
a_swale = 1 - g_swale - v_swale

wabila_roof_to_swale_results <- c(a_swale, g_swale, v_swale)
# ??? how to take into account cascading effects and dimension of swale???

# compare result in R-ABIMO ----------------------------------------------------

# R-ABIMO results
result_simple_roof <- select_columns(
  run_rabimo(data = simple_roof_area, config = config,
             simulate_abimo = FALSE, check = FALSE),
  c("ROW","RI","VERDUNSTUN"))

result_green_roof <-  select_columns(
  run_rabimo(data = green_roof_area, config = config,
             simulate_abimo = FALSE, check = FALSE),
  c("ROW","RI","VERDUNSTUN"))

result_paved_area <-  select_columns(
  run_rabimo(data = paved_area, config = config,
             simulate_abimo = FALSE, check = FALSE),
  c("ROW","RI","VERDUNSTUN"))

result_roof_to_swale <- select_columns(
  run_rabimo(data = area_to_swale, config = config,
             simulate_abimo = FALSE, check = FALSE),
  c("ROW","RI","VERDUNSTUN"))

# express results in percentage and compare with WABILA
factors_simple_roof <- result_simple_roof %>%
  magrittr::divide_by(simple_roof_area$prec_yr) %>%
  setNames(c("runoff","infiltration","evaporation")) %>%
  rbind(wabila_roof_results) %>%
  as.matrix() %>%
  magrittr::set_rownames(c("rabimo", "wabila"))

factors_green_roof <- result_green_roof %>%
  magrittr::divide_by(green_roof_area$prec_yr) %>%
  setNames(c("runoff","infiltration","evaporation")) %>%
  rbind(wabila_green_roof_results) %>%
  as.matrix() %>%
  magrittr::set_rownames(c("rabimo", "wabila"))

factors_paved_area <- result_paved_area %>%
  magrittr::divide_by(paved_area$prec_yr) %>%
  setNames(c("runoff","infiltration","evaporation")) %>%
  rbind(wabila_paved_results) %>%
  as.matrix() %>%
  magrittr::set_rownames(c("rabimo", "wabila"))

factors_roof_to_swale <- result_roof_to_swale %>%
  magrittr::divide_by(area_to_swale$prec_yr) %>%
  setNames(c("runoff","infiltration","evaporation")) %>%
  rbind(wabila_roof_to_swale_results) %>%
  as.matrix() %>%
  magrittr::set_rownames(c("rabimo", "wabila"))

print(factors_simple_roof)
print(factors_green_roof)
print(factors_paved_area)
print(factors_roof_to_swale)

