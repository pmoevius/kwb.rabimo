# get_climate ------------------------------------------------------------------
# provides climate relevant input data

get_climate <- function(input){
  climate <- select_columns(input, c(
    "prec_yr",
    "prec_s",
    "epot_yr",
    "epot_s",
  ))
  climate[["x_ratio"]] <- climate[["prec_yr"]] / climate[["epot_yr"]]
  climate
}
