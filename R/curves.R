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
