# DEPENDENCIES ----------------------------------------------------------------
library(magrittr)
library(anthro)

# CORE GROWTH CURVE LOGIC -----------------------------------------------------

#' Get the WHO standard weight-for-age percentile for a young child
#'
#' @param sex A string: "m", "f" or their capital forms.
#' @param weight_kg A number: the child's weight in kilograms.
#' @param age_days A number representing the child's age in days.
#' @return A number in [0, 100), representing the WHO standard growth
#'   percentile
#' @examples
#' percentile_weight("F", 3.930, 0)
percentile_weight <- function(sex, weight_kg, age_days) {
  100.0 * (
    anthro_zscores(
      sex,
      age = age_days,
      is_age_in_month = FALSE,
      weight = weight_kg) %>%
    .$zwei %>%
    pnorm())
}

#' Get the WHO standard length-for-age percentile for a young child
#'
#' @param sex A string: "m", "f" or their capital forms.
#' @param length_cm A number: the child's length in centimeters.
#' @param age_days A number representing the child's age in days.
#' @return A number in [0, 100), representing the WHO standard growth
#'   percentile
#' @examples
#' percentile_length("F", 54 6, 0)
percentile_length <- function(sex, length_cm, age_days) {
  100.0 * (
    anthro_zscores(
      sex,
      age = age_days,
      is_age_in_month = FALSE,
      lenhei = length_cm) %>%
    .$zlen %>%
    pnorm())
}

#' Get the WHO standard head-circumference-for-age percentile for a young child
#'
#' @param sex A string: "m", "f" or their capital forms.
#' @param circumference_cm A number: the child's head circumference in
#'   centimeters.
#' @param age_days A number representing the child's age in days.
#' @return A number in [0, 100), representing the WHO standard growth
#'   percentile
#' @examples
#' percentile_head("F", 36, 0)
percentile_head <- function(sex, circumference_cm, age_days) {
  100.0 * (
    anthro_zscores(
      sex,
      age = age_days,
      is_age_in_month = FALSE,
      headc = circumference_cm) %>%
    .$zhc %>%
    pnorm())
}
