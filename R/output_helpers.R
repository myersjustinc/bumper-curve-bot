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
