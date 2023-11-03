# realEvapoTranspiration -------------------------------------------------------

#' Calculate Actual Evapotranspiration with Bagrov
#'
#' @param precipitation precipitation in mm
#' @param potentialEvaporation potential evaporation in mm
#' @param bagrovParameter Bagrov parameter (n-value)
#' @param xRatio optional. Instead of \code{precipitation} and
#'   \code{potentialEvaporation} the quotient of both may be passed to this
#'   function. The idea is to calculate the quotient out of the function and to
#'   reuse the quotient instead of recalculating it.
#' @param FUN_yRatio function to be called to calculate the yRatio(s) from the
#'   given xRatio(s). Default: \code{kwb.rabimo:::yRatio_3}
#' @param \dots further arguments passed to \code{FUN_yRatio}
#' @return estimated actual evapotranspiration in mm
#' @importFrom parallel detectCores makeCluster parLapply stopCluster
#' @export
realEvapoTranspiration <- function(
    precipitation, # P or P + KR + BER
    potentialEvaporation, # ETP
    bagrovParameter, # n
    xRatio = NULL,
    FUN_yRatio = yRatio_3,
    ...
)
{
  # Calculate the x-factor of the Bagrov relation
  if (is.null(xRatio)) {
    xRatio <- precipitation / potentialEvaporation
  }

  # Estimate the y-ratio (of real evaporation to potential evaporation)...

  # ... and calculate the real evapotransporation using the estimated y-ratio
  FUN_yRatio(bagrovParameter, xRatio, ...) * potentialEvaporation
}

# yRatio -----------------------------------------------------------------------
yRatio <- function(bagrovParameter, xRatio)
{
  df <- calculate_bagrov_table(n_values = bagrovParameter)

  # print_if(TRUE, nrow(df), caption = "chatty's nrow(df)")

  abs_diffs <- abs(df$P_over_E_p - xRatio)

  index <- which.min(abs_diffs)

  #cat("min(abs_diffs): ", abs_diffs[index], "\n")

  df$E_a_over_E_p[index]
}

# yRatio_2 ---------------------------------------------------------------------
yRatio_2 <- function(bagrovParameter, xRatio)
{
  df <- kwb.abimo::calculate_bagrov_curve(
    effectivity = bagrovParameter,
    P_over_Ep_max = xRatio + 0.1,
    delta_Ea = 1
  )

  # print_if(TRUE, nrow(df))

  abs_diffs <- abs(df$P_over_Ep - xRatio)

  index <- which.min(abs_diffs)

  #cat("min(abs_diffs): ", abs_diffs[index], "\n")

  df$Ea_over_Ep[index]
}

# yRatio_3 ---------------------------------------------------------------------

#' Lookup yRatio for given xRatio on a BAGROV curve
#'
#' @param bagrovParameter (vector of) BAGROV parameter(s)
#' @param xRatio (vector of) x-ratio(s) (between precipitation and potential
#'   evaporation) for which to look up the corresponding y-ratio(s) (between
#'  actual evaporation and potential evaporation) on the BAGROV curve(s)
#' @param minSizeForParallel minimum number of BAGROV curves to start
#'   parallel processing
#' @param useAbimoAlgorithm whether or not to use the original algorithm that
#'   is implemented in the C++ code (converted to R:
#'   \code{kwb.rabimo:::yratio_cpp}). Default: \code{FALSE}
yRatio_3 <- function(
    bagrovParameter,
    xRatio,
    minSizeForParallel = 10L,
    useAbimoAlgorithm = FALSE
)
{
  if (length(bagrovParameter) == 1L) {
    stopifnot(length(xRatio) == 1L)
    return(
      if (useAbimoAlgorithm) {
        yRatio_2(bagrovParameter, xRatio)
      } else {
        yratio_cpp(bagrovParameter, xRatio)
      }
    )
  }

  # Split combinations of bagrovParameter and xRatio into blocks with
  # the same bagrovParameter
  combis <- data.frame(
    bagrovParameter = bagrovParameter,
    xRatio = xRatio,
    index = seq_along(bagrovParameter)
  )

  combisets <- split(combis, combis$bagrovParameter)

  if (useAbimoAlgorithm) {

    result <- combisets %>%
      lapply(function(df) {
        df[["yRatio"]] <- yratio_cpp(
          bag = df[["bagrovParameter"]][1L],
          x = df[["xRatio"]]
        )
        df
      }) %>%
      do.call(what = rbind)

  } else {

    P_over_Ep_max <- max(xRatio) + 0.1

    # Define function to be called for each combiset
    getBagrovCurve <- function(combiset) {
      bagrov <- combiset$bagrovParameter[1L]
      cat_and_run(
        paste("Calculating BAGROV curve for BAGROV parameter =", bagrov),
        kwb.abimo::calculate_bagrov_curve(
          effectivity = bagrov,
          P_over_Ep_max = P_over_Ep_max,
          delta_Ea = 1
        )
      )
    }

    # Calculate the BAGROV curves for the different bagrovParameter values

    # Should we do parallel processing?
    doParallel <- length(combisets) >= minSizeForParallel &&
      (ncores <- parallel::detectCores() - 1L) > 1L

    curves <- if (doParallel) {

      # Prepare parallel processing
      cl <- parallel::makeCluster(ncores)
      on.exit(parallel::stopCluster(cl))

      cat_and_run(
        sprintf(
          "Calculating %d BAGROV curves in parallel on %d cores",
          length(combisets),
          ncores
        ),
        expr = {
          # Call the call_with_data function in a (parallel) loop
          parallel::parLapply(cl, combisets, fun = getBagrovCurve)
        }
      )

    } else {

      cat_and_run(
        sprintf("Calculating %d BAGROV curves", length(combisets)),
        newLine = 3L,
        expr = {
          # Calculate the Bagrov curves for the different bagrovParameter values
          lapply(combisets, FUN = getBagrovCurve)
        }
      )
    }

    result <- do.call(rbind, mapply(
      FUN = function(curve, combiset) {
        cat_and_run(
          sprintf(
            "Reading %d yRatio values from the BAGROV curve (n = %f)",
            nrow(combiset),
            combiset$bagrovParameter[1L]
          ),
          expr = {
            n <- nrow(curve)
            M <- matrix(rep(combiset$xRatio, each = n), nrow = n)
            indices <- apply(abs(curve$P_over_Ep - M), 2L, which.min)
            cbind(
              index = combiset$index,
              yRatio = curve$Ea_over_Ep[indices]
            )
          }
        )
      },
      curves,
      combisets,
      SIMPLIFY = FALSE
    ))
  }

  # initialise result vector
  yratios <- numeric(length(bagrovParameter))

  # set the yRatio values at the indices of the related xRatio input values
  yratios[result[, "index"]] <- result[, "yRatio"]

  yratios
}

# calculate_bagrov_table -------------------------------------------------------
calculate_bagrov_table <- function(n_values) {

  # test
  # n_values <- bagrovParameter

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
