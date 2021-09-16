# DEPENDENCIES ----------------------------------------------------------------
library(tidyverse)
library(anthro)
library(units)

# CORE CONVERSION LOGIC -------------------------------------------------------

#' Get the WHO standard weight-for-age percentile for a young child
#'
#' @param sex A string: "m", "f" or their capital forms.
#' @param weight_kg A number: the child's weight in kilograms.
#' @param age A number representing the child's age. See next argument for
#'   units.
#' @param is_age_in_month A boolean: TRUE if the preceding argument is an age
#'   in months, or FALSE if it is an age in days.
#' @return A number in [0, 100), representing the WHO standard growth
#'   percentile
#' @examples
#' percentile_weight_kg("F", 3.930, 0, FALSE)
#' @family weight-for-age percentile functions
#' @seealso \code{\link{percentile_weight_g}} for weights in grams,
#'   \code{\link{percentile_weight_lb_oz}} for weights in pounds and ounces.
percentile_weight_kg <- function(sex, weight_kg, age, is_age_in_month = TRUE) {
  100.0 * (
    anthro_zscores(
      sex,
      age = age,
      is_age_in_month = is_age_in_month,
      weight = weight_kg) %>%
    .$zwei %>%
    pnorm())
}

# DIFFERENT WEIGHT UNITS ------------------------------------------------------

#' Get the WHO standard weight-for-age percentile for a young child
#'
#' @param sex A string: "m", "f" or their capital forms.
#' @param weight_g A number: the child's weight in grams.
#' @param age A number representing the child's age. See next argument for
#'   units.
#' @param is_age_in_month A boolean: TRUE if the preceding argument is an age
#'   in months, or FALSE if it is an age in days.
#' @return A number in [0, 100), representing the WHO standard growth
#'   percentile
#' @examples
#' percentile_weight_g("F", 3930, 0, FALSE)
#' @family weight-for-age percentile functions
#' @seealso \code{\link{percentile_weight_kg}} for weights in kilograms,
#'   \code{\link{percentile_weight_lb_oz}} for weights in pounds and ounces.
percentile_weight_g <- function(sex, weight_g, age, is_age_in_month = TRUE) {
  weight_kg <- weight_g %>%
    set_units("gram") %>%
    set_units("kilogram") %>%
    drop_units()
  percentile_weight_kg(sex, weight_kg, age, is_age_in_month)
}

#' Get the WHO standard weight-for-age percentile for a young child
#'
#' @param sex A string: "m", "f" or their capital forms.
#' @param weight_lb A number: the portion of the child's weight in pounds
#' @param weight_oz A number: the portion of the child's weight in ounces
#' @param age A number representing the child's age. See next argument for
#'   units.
#' @param is_age_in_month A boolean: TRUE if the preceding argument is an age
#'   in months, or FALSE if it is an age in days.
#' @return A number in [0, 100), representing the WHO standard growth
#'   percentile
#' @examples
#' percentile_weight_kg("F", 3.930, 0, FALSE)
#' @family weight-for-age percentile functions
#' @seealso \code{\link{percentile_weight_g}} for weights in grams,
#'   \code{\link{percentile_weight_kg}} for weights in kilograms.
percentile_weight_lb_oz <- function(
    sex, weight_lb, weight_oz, age, is_age_in_month = TRUE) {
  weight_lb <- weight_lb %>%
    set_units("avoirdupois_pound")
  weight_oz <- weight_oz %>%
    set_units("avoirdupois_ounce")
  weight_kg <- (weight_lb + weight_oz) %>%
    set_units("kilogram") %>%
    drop_units()
  percentile_weight_kg(sex, weight_kg, age, is_age_in_month)
}
