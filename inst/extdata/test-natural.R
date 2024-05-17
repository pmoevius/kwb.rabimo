# Script for calculation of the natural water balance and the deviation from
# the natural water balance Delta-W

# load data
inputs <- kwb.rabimo::rabimo_inputs_2020
results <- kwb.utils:::get_cached("rabimo_results_2020")

# Provide all functions from kwb.rabimo in the global environment
kwb.utils::assignPackageObjects("kwb.rabimo")

# simulate_abimo = FALSE;check = FALSE
# `%>%` <- magrittr::`%>%`

# Convert the "status quo" input data to input data representing different types
# of "natural states"
status_quo <- select_elements(inputs, "data")

nat_input_datasets <- list(
  undeveloped = data_to_natural(status_quo, type = "undeveloped"),
  forested = data_to_natural(status_quo, type = "forested"),
  horticultural = data_to_natural(status_quo, type = "horticultural")
)

# Calculate R-Abimo results for the natural states
nat_results <- lapply(nat_input_datasets, function(data) {
  run_rabimo(
    data = data,
    config = inputs$config,
    check = FALSE,
    intermediates = TRUE,
    simulate_abimo = FALSE
  )
})

# Simulate the selection of some blocks
urban_data <- dplyr::sample_n(results, 7)

microbenchmark::microbenchmark(
  dw1 = calculate_delta_W(
    natural = nat_results$undeveloped,
    urban = urban_data,
    return_codes = TRUE
  ),
  dw2 = calculate_delta_W_2(
    natural = nat_results$undeveloped,
    urban = urban_data
  ),
  check = "identical"
)

identical(dw1, dw2)

# compare natural results
combined_results <- kwb.utils::rbindAll(nat_results, nameColumn = "source")

combined_results_long <- combined_results %>%
  tidyr::pivot_longer(
    cols = c(surface_runoff, infiltration, evaporation),
    names_to = "variable",
    values_to = "value"
  ) %>%
  as.data.frame() %>%
  dplyr::filter(variable != "surface_runoff")

land_type_colors <- c(
  "undeveloped" = "coral2",
  "forested" = "darkolivegreen3",
  "horticultural" = "cornflowerblue"
)

# all berlin areas
nat_res_plot_all <- combined_results_long %>%
  ggplot2::ggplot(mapping =  ggplot2::aes(x = value, fill = source)) +
  ggplot2::geom_histogram(binwidth = 5, position = "dodge") +
  ggplot2::facet_wrap(~ variable, scale = "free") +
  ggplot2::labs(title = "Comparison of Different Natural Water Balance Scenarios") +
  ggplot2::scale_fill_manual(values = land_type_colors) +
  ggplot2::theme_minimal()

nat_res_plot_all

# exclude water bodies and forests
exclude_patterns <- c("water", "forest")

filtered_input_codes <- inputs$data %>%
  dplyr::mutate(block_type = tolower(block_type)) %>%
  dplyr::filter(!grepl(paste(exclude_patterns, collapse = "|"), block_type)) %>%
  dplyr::select(code) %>%
  dplyr::pull()

filtered_results_long <- combined_results_long %>%
  dplyr::filter(code %in% filtered_input_codes)

nat_res_plot_no_water_no_forest <- filtered_results_long %>%
  ggplot2::ggplot(mapping =  ggplot2::aes(x = value, fill = source)) +
  ggplot2::geom_histogram(binwidth = 5, position = "dodge") +
  ggplot2::facet_wrap(~ variable, scale = "free") +
  ggplot2::labs(title = "Comparison of Different Natural Water Balance Scenarios (Excluding Water Bodies and Forests)") +
  ggplot2::scale_fill_manual(values = land_type_colors) +
  ggplot2::theme_minimal()

nat_res_plot_no_water_no_forest

area_list <- list(
  developed_area = "developed" %>%
    generate_rabimo_area(),
  dev_forested_area = "dev_forested" %>%
    generate_rabimo_area(land_type = "forested"),
  dev_horticultural_area = "dev_horticultural" %>%
    generate_rabimo_area(land_type = "horticultural"),
  nat_undeveloped_area = "undeveloped" %>%
    generate_rabimo_area() %>%
    data_to_natural(),
  nat_forested_area = "nat_forested" %>%
    generate_rabimo_area(land_type = "forested") %>%
    data_to_natural(),
  nat_horticultural_area = "nat_horticultural" %>%
    generate_rabimo_area(land_type = "horticultural") %>%
    data_to_natural()
)

area_results <- lapply(area_list, function(data) {
  run_rabimo(
    data = data,
    config = inputs$config,
    check = FALSE,
    intermediates = TRUE,
    simulate_abimo = FALSE
  )
}) %>%
  kwb.utils::rbindAll()
