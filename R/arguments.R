# DEPENDENCIES ----------------------------------------------------------------
library(tidyverse)

# WEIGHT STANDARDIZATION ------------------------------------------------------

#' Convert a user-provided weight to kilograms, totaling several weights if
#' needed.
#'
#' @param raw_text The exact string provided by the user when prompted for a
#'   child's sex.
#' @return A number representing the total weight in kilograms
#' @examples
#' standardize_weight("3930 g")
#' standardize_weight("8 lb 10 oz")
standardize_weight <- function(raw_text) {
  # TODO: Add this.
}

# SEX STANDARDIZATION ---------------------------------------------------------

SEX_LOOKUPS <- tribble(
  ~raw_lower,   ~standardized,
  "male",       "M",
  "female",     "F",
  "m",          "M",
  "f",          "F",
  "boy",        "M",
  "girl",       "F",
  "b",          "M",
  "g",          "F")

#' Convert a user-provided sex to a form recognized by the `anthro` package.
#'
#' @param raw_text The exact string provided by the user when prompted for a
#'   child's sex.
#' @return A string: "M" or "F"
#' @examples
#' standardize_sex("boy")
standardize_sex <- function(raw_text) {
  clean <- raw_text %>%
    str_to_lower() %>%
    str_remove_all("\\s") %>%
    str_trim()
  SEX_LOOKUPS %>%
    filter(raw_lower == clean) %>%
    head(1) %>%
    .$standardized
}

# AGE STANDARDIZATION ---------------------------------------------------------

#' Convert a user-provided age to a form recognized by the `anthro` package.
#'
#' @param raw_text The exact string provided by the user when prompted for a
#'   child's age
#' @return A named list with two elements: `amount` (numeric) and `is_months`
#'   (logical; TRUE if `amount` is a number of months, FALSE if it's a number
#'   of days)
#' @examples
#' standardize_age("6 weeks")
standardize_age <- function(raw_text) {
  # TODO: Add this.
}
