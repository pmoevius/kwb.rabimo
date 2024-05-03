# Script for calculation of the natural water balance and the deviation from
# the natural water balance Delta-W

# load data
inputs <- rabimo_inputs_2020
results <- kwb.utils:::get_cached("rabimo_results_2020")

# get useful functions
# kwb.utils::assignPackageObjects("kwb.rabimo");
# simulate_abimo = FALSE;check = FALSE
`%>%` <- magrittr::`%>%`

# modify inputs to simulate the natural state
nat_input_data_1 <- inputs$data %>%
  dplyr::mutate(roof = 0, pvd = 0, pvd_rd = 0, sealed = 0, irrigation = 0)

nat_input_data_2 <- inputs$data %>%
  dplyr::mutate(
    roof = 0, pvd = 0, pvd_rd = 0, sealed = 0, irrigation = 0, land_type = "forested"
  )

nat_input_data_3 <- inputs$data %>%
  dplyr::mutate(
    roof = 0, pvd = 0, pvd_rd = 0, sealed = 0, irrigation = 0, land_type = "horticultural"
  )

# calculate water balance for natural scenario(S)
nat_result_1 <- run_rabimo(data = nat_input_data_1, config = inputs$config,
                           check = FALSE, intermediates = TRUE, simulate_abimo = FALSE)

nat_result_2 <- run_rabimo(data = nat_input_data_2, config = inputs$config,
                           check = FALSE, intermediates = TRUE, simulate_abimo = FALSE)

nat_result_3 <- run_rabimo(data = nat_input_data_3, config = inputs$config,
                           check = FALSE, intermediates = TRUE, simulate_abimo = FALSE)


# compare natural results
nat_result_1$source <- "undeveloped"
nat_result_2$source <- "forested"
nat_result_3$source <- "horticultural"

combined_results <- rbind(nat_result_1, nat_result_2, nat_result_3)

combined_results_long <- combined_results %>%
  tidyr::pivot_longer(cols = c(total_surface_runoff, total_infiltration, total_evaporation),
                      names_to = "variable",
                      values_to = "value") %>%
  as.data.frame() %>%
  dplyr::filter(variable != "total_surface_runoff")

land_type_colors <- c("undeveloped" = "coral2",
                      "forested" = "darkseagreen",
                      "horticultural" = "cadetblue3")

nat_res_plot <-
  ggplot2::ggplot(data = combined_results_long,
                  mapping =  ggplot2::aes(x = value, fill = source)) +
  ggplot2::geom_histogram(binwidth = 1, position = "dodge") +
  ggplot2::facet_wrap(~ variable, scale = "free") +
  ggplot2::labs(title = "Comparison of Different Natural Water Balance Scenarios") +
  ggplot2::scale_fill_manual(values = land_type_colors) +
  ggplot2::theme_minimal()


# calculate_delta_W ------------------------------------------------------------
# natural <- nat_result_2[,-ncol(nat_result_1)]
# urban <- head(results)

calculate_delta_W <- function(natural, urban,
                              cols_to_omit = c("total_area"),
                              return_codes = FALSE)
{

  stopifnot("code" %in% names(natural))
  stopifnot("code" %in% names(urban))
  stopifnot(all(urban[["code"]] %in% natural[["code"]]))

  combined <- dplyr::left_join(urban,
                               kwb.utils::removeColumns(natural, cols_to_omit),
                               by = "code", suffix = c("_u","_n"))

  # variable columns
  column_names <- grep("_.$", names(combined), value = TRUE)
  unique_names <- unique(sub("_.$", "", column_names))

  precipitation <- rowSums(combined[,column_names])/2

  diff_list <- list()

  for (name in unique_names) {
    urban_name <- paste0(name, "_u")
    natural_name <- paste0(name, "_n")

    diff_list[[name]] <- abs(combined[[urban_name]] - combined[[natural_name]])
  }

  delta_w <- round(rowSums(as.data.frame(diff_list))/2*100/precipitation,1)

  if(return_codes){
    data.frame(code = urban[["code"]], delta_w)
  } else {
    data.frame(delta_w)
  }
}

