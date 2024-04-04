# Test script for implementing stormwater management in RABIMO

# rabimo_input_2020 <- readRDS(file = "C:/development/r-projects/kwb.rabimo/inst/extdata/inputs_2020.rds")
# usethis::use_data(rabimo_input_2020)

kwb.utils::assignPackageObjects("kwb.rabimo")
`%>%` <- magrittr::`%>%`

input <- kwb.rabimo::rabimo_inputs_2020

input_data <- input$data
input_config <- input$config

# subset input for testing
codes <- c("0000000004000011","0000000004000017","0000000004000018",
           "0000000004000034","0000000004000876","0000000004000882",
           "0000000004000895","0000000004001658","0000000004001680",
           "0000000004000896","0000000004002242","0000000007000617",
           "0000000007000630","0000000007000631","0000000007000642",
           "0000000007001516","0000000007001529","0000000007002218",
           "0000000007002226","0000000007001539","0000000007002227",
           "0000000007002402","0900452811000000","0900452941000000",
           "0900453141000000","0900453151000000","0900453171000000",
           "0900453181000000","0900453191000000","0900453201000000",
           "1100612741000000","1100612751000000","1100612861000000",
           "1100612871000000","1100612881000000","1100613031000000",
           "1100613041000000")

input_data <- input_data[which(input_data$code %in% codes),]

## GREEN-ROOF ------------------------------------------------------------------
current_bagrov <- input_config$bagrov_values

# add green roof parameters to config
input_config$bagrov_values <- c(current_bagrov[1],
                                "green_roof" = 0.65,
                                current_bagrov[2:length(current_bagrov)])


# add column "green_roof" [must also happen in prepare_berlin..()]
input_data <- input_data %>%
  dplyr::mutate(green_roof = 0) %>%
  dplyr::relocate(green_roof, .after = roof)

# fake storm-water management input from user: green_roof
set.seed(123)
green_roof_input <- data.frame(code = codes) %>%
  dplyr::mutate(green_roof = round(runif(length(codes)),2)) %>%
  dplyr::sample_frac(0.6)

updated_input_data <- input_data %>%
  dplyr::left_join(green_roof_input, by = "code", suffix = c("",".new")) %>%
  dplyr::mutate(green_roof = dplyr::coalesce(green_roof.new, green_roof)) %>%
  dplyr::select(-green_roof.new)

# calculate water balance results with green-roof component
results_gr <- kwb.rabimo::run_rabimo(data = updated_input_data,
                                     config = input_config,
                                     check = FALSE)

results_no_gr <- kwb.rabimo::run_rabimo(data = input_data,
                                        config = input_config,
                                        check = FALSE)

compare <- cbind(select_columns(updated_input_data,
                                c("code", "roof", "green_roof")),
                 results_gr[-1] - results_no_gr[-1])

compare <- updated_input_data %>%
  select_columns(c("code", "roof", "green_roof")) %>%
  cbind(results_gr[-1] - results_no_gr[-1])







