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

#' Given a weight in grams, return the equivalent weight in kilograms.
#'
#' @param grams A weight in grams.
#' @return A weight in kilograms
#' @examples
#' weight_g(3930)
#' @family weight conversion functions
#' @seealso \code{\link{weight_lb_oz}} for weights in pounds and/or ounces.
weight_g <- function(grams) {
  grams %>%
    set_units("gram") %>%
    set_units("kilogram") %>%
    drop_units()
}

#' Given a weight in pounds and/or ounces, return the equivalent weight in
#' kilograms.
#'
#' @param pounds A weight in pounds.
#' @param ounces A weight in ounces.
#' @return A weight in kilograms
#' @examples
#' weight_lb_oz(8, 10)
#' weight_lb_oz(8.625, 0)
#' weight_lb_oz(0, 138)
#' @family weight conversion functions
#' @seealso \code{\link{weight_g}} for weights in grams.
weight_lb_oz <- function(pounds, ounces) {
  pounds <- pounds %>%
    set_units("avoirdupois_pound")
  ounces <- ounces %>%
    set_units("avoirdupois_ounce")
  (pounds + ounces) %>%
    set_units("kilogram") %>%
    drop_units()
}
