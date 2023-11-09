# yearly_height_to_volume_flow -------------------------------------------------

#' Convert Yearly Height (mm) to Volume Flow (unit?)
#'
#' @param height height in mm
#' @param area area in square metres
#' @export
yearly_height_to_volume_flow <- function(height, area)
{
  height * 3.171 * area / 100000.0
}
