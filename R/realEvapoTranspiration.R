# realEvapoTranspiration -------------------------------------------------------

#' Calculate Actual Evapotranspiration with Bagrov
#'
#' @param precipitation precipitation in mm
#' @param potentialEvaporation potential evaporation in mm
#' @param efficiency so-called Bagrov value
#' @return estimated actual evapotranspiration in mm
#' @export
realEvapoTranspiration <- function(
    precipitation, # P or P + KR + BER
    potentialEvaporation, # ETP
    efficiency # n
)
{
  # test
  #precipitation <- precipitation$perYearCorrectedFloat
  #potentialEvaporation <- potentialEvaporation$perYearFloat
  #efficiency <- config$bagrovValues["Dachflaechen"]

  # Calculate the x-factor of the Bagrov relation
  xRatio <- precipitation / potentialEvaporation

  # Estimate the y-ratio (of real evaporation to potential evaporation)...

  #y_ratio_1 <- yRatio(efficiency, xRatio)
  y_ratio_2 <- yRatio_2(efficiency, xRatio)

  #stopifnot(identical(y_ratio_1, y_ratio_2))

  # ... and calculate the real evapotransporation using the estimated y-ratio
  y_ratio_2 * potentialEvaporation
}

# yRatio -----------------------------------------------------------------------
yRatio <- function(efficiency, xRatio)
{
  df <- calculate_bagrov_table(n_values = efficiency)

  # print_if(TRUE, nrow(df), caption = "chatty's nrow(df)")

  abs_diffs <- abs(df$P_over_E_p - xRatio)

  index <- which.min(abs_diffs)

  #cat("min(abs_diffs): ", abs_diffs[index], "\n")

  df$E_a_over_E_p[index]
}

# yRatio_2 ---------------------------------------------------------------------
yRatio_2 <- function(efficiency, xRatio)
{
  df <- kwb.abimo::calculate_bagrov_curve(
    effectivity = efficiency,
    P_over_Ep_max = xRatio + 0.1,
    delta_Ea = 1
  )

  # print_if(TRUE, nrow(df))

  abs_diffs <- abs(df$P_over_Ep - xRatio)

  index <- which.min(abs_diffs)

  #cat("min(abs_diffs): ", abs_diffs[index], "\n")

  df$Ea_over_Ep[index]
}

# calculate_bagrov_table -------------------------------------------------------
calculate_bagrov_table <- function(n_values) {

  # test
  # n_values <- efficiency

  # Initialize values
  E_a <- 0
  P <- 0
  E_p <- 650  # mm/a
  P_limit <- 4 * E_p  # Si arriva fino a 4 * E_p
  Delta_E_a <- 1
  tolerance <- 0.01

  # empty dataframe for results
  results <- data.frame(
    E_a_over_E_p = numeric(),
    P_over_E_p = numeric(),
    n_value = numeric()
  )

  # loop over all given n_values
  for (n in n_values) {

    #test
    #n <- n_values

    # reset E_a and P for every new n_value
    E_a <- 0
    P <- 0

    # loop until the difference between P and P_limit is either lower than the
    # tolerance or E_a/E_p > 1
    while(abs(P - P_limit) > tolerance & E_a / E_p <= 1) {

      Delta_P <- Delta_E_a / (1 - (E_a / E_p)^n)
      P <- P + Delta_P
      E_a <- E_a + Delta_E_a

      # normalize results and add to result dataframe
      results <- rbind(results, data.frame(
        E_a_over_E_p = E_a / E_p,
        P_over_E_p = P / E_p,
        n_value = n
      ))
    }
  }

  return(results)
}
