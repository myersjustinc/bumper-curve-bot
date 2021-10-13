# CONSTANTS ---------------------------------------------------------
ORDINAL_SUFFIXES <- list(
  "0" = "th", "1" = "st", "2" = "nd", "3" = "rd", "4" = "th",
  "5" = "th", "6" = "th", "7" = "th", "8" = "th", "9" = "th")

# HELPERS ---------------------------------------------------------------------

#' Format a percentile as a more readable string.
#'
#' @param percentile A number in [0, 100).
#' @return A string
format_percentile <- function(percentile) {
  rounded <- round(percentile, 1)
  tenths <- round((rounded %% 1) * 10)
  final_digit <- dplyr::if_else(tenths == 0, rounded %% 10, tenths)
  suffix <- ORDINAL_SUFFIXES[[as.character(final_digit)]]
  stringr::str_c(rounded, suffix)
}

#' Generate a sentence about a given percentile calculation.
#'
#' @param percentile A number in [0,100).
#' @param curve_options A nested list as returned by `parse_options_curve` in
#'   `R/discord_helpers.R`.
#' @return A string
format_curve <- function(percentile, curve_options) {
  stringr::str_c(
    "At ", attr(curve_options$age, "explained"), ", a ",
    dplyr::if_else(curve_options$sex == "M", "boy", "girl"), " ",
    dplyr::case_when(
      curve_options$type == "weight" ~ stringr::str_c(
        "weighing ", attr(curve_options$weight, "explained")),
      curve_options$type == "length" ~ stringr::str_c(
        attr(curve_options$length, "explained"), " long"),
      curve_options$type == "head" ~ stringr::str_c(
        "with a head circumference of ",
        attr(curve_options$circumference, "explained")),
    ), " ",
    "is at the **",
    format_percentile(percentile),
    " ", "percentile**."
  )
}
