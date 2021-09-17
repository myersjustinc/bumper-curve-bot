# DEPENDENCIES ----------------------------------------------------------------
library(tidyverse)

# USED MULTIPLE PLACES --------------------------------------------------------

NUMBERS <- tribble(
  ~text,        ~value,
  "one",        1,
  "two",        2,
  "three",      3,
  "four",       4,
  "five",       5,
  "six",        6,
  "seven",      7,
  "eight",      8,
  "nine",       9,
  "ten",        10,
  "eleven",     11,
  "twelve",     12,
  "thirteen",   13,
  "fourteen",   14,
  "fifteen",    15,
  "sixteen",    16,
  "seventeen",  17,
  "eighteen",   18,
  "nineteen",   19,
  "twenty",     20)
NUMBERS_REPLACEMENTS <- NUMBERS %>%
  transmute(text, value = as.character(value)) %>%
  deframe()

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

AGE_LOOKUPS <- tribble(
  ~unit,    ~days,
  "day",    1,
  "week",   7,
  # For growth curves, "1 month = 30.4375 days" (i.e., 365.25 / 12), per:
  # https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-age/instructionsa1066ff7424c40fa9282d0e6623dc097.pdf?sfvrsn=334a2f9a_0
  "month",  30.4375,
  "year",   365.25)
AGE_ABBREVIATIONS <- tribble(
  ~unit,    ~abbr,
  "day",    "day",
  "day",    "dy",
  "day",    "d",
  "week",   "week",
  "week",   "wk",
  "week",   "w",
  "month",  "month",
  "month",  "mon",
  "month",  "mo",
  "month",  "m",
  "year",   "year",
  "year",   "yr",
  "year",   "y")

#' Convert a user-provided age to a form recognized by the `anthro` package.
#'
#' @param raw_text The exact string provided by the user when prompted for a
#'   child's age
#' @return A number of days equivalent to the given age
#' @examples
#' standardize_age("6 weeks")
standardize_age <- function(raw_text) {
  with_digits <- raw_text %>%
    str_to_lower() %>%
    str_replace_all(NUMBERS_REPLACEMENTS)
  age_pattern <- regex("([0-9.]+)\\s*([a-z]+)", ignore_case = TRUE)
  ages_raw <- with_digits %>%
    str_match_all(age_pattern) %>%
    .[[1]]
  colnames(ages_raw) <- c("full", "amount", "unit")
  ages_raw <- ages_raw %>%
    as_tibble() %>%
    transmute(
      amount = as.numeric(amount),
      unit_raw = str_remove(unit, "[sS]$"))
  ages_raw %>%
    inner_join(AGE_ABBREVIATIONS, by = c("unit_raw" = "abbr")) %>%
    inner_join(AGE_LOOKUPS, by = "unit") %>%
    summarize(total_days = sum(amount * days)) %>%
    .$total_days
}
