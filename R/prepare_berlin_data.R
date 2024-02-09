# prepare_berlin_data ----------------------------------------------------------

#' Function to be called when using ABIMO with the 'old' Berlin-data structure
#'
#' It calls the functions \code{\link{prepare_input_data}} and
#' \code{\link{abimo_config_to_config}} and modifies the config-object to the required
#' structure.
#'
#' @param data_file a path for the .dbl file containing the input data
#' @param config_file a path for the .XML config file
#' @return a list object containing elements \code{data},
#'   the prepared input data and \code{config}, the prepared config object,
#'   both in a form readable by \code{\link{run_rabimo}}
#' @export

prepare_berlin_data <- function(data_file,
                                config_file = kwb.abimo::default_config())
{

  # read XML-config file
  config <- kwb.abimo::read_config(file = config_file)
  config <- abimo_config_to_config(config)

  # read dbl-file
  data <- foreign::read.dbf(data_file)

  # prepare input data
  data <- prepare_input_data(data, config)

  # prepare config object
  #config <- prepare_config(config) # to be written...

  # create list object with prepared RABIMO-input-data and config-object
  berlin_inputs <- list(data, config)

  return(list(berlin_inputs))

}
