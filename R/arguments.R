# DEPENDENCIES ----------------------------------------------------------------
library(tidyverse)
library(units)

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

WEIGHT_LOOKUPS <- tribble(
    ~unit,        ~canonical,   ~udunits_key,
    "kilogram",   "kg",         "kilogram",
    "gram",       "g",          "gram",
    "pound",      "lb",         "avoirdupois_pound",
    "ounce",      "oz",         "avoirdupois_ounce") %>%
  pmap_dfr(function(...) {
    lookup <- tibble(...)
    kg_equiv <- 1.0 %>%
      as_units(lookup$udunits_key) %>%
      set_units("kilogram") %>%
      drop_units()
    lookup %>%
      mutate(kg_equiv = kg_equiv)
  })
WEIGHT_ABBREVIATIONS <- tribble(
  ~unit,        ~abbr,
  "kilogram",   "kilogram",
  "kilogram",   "kilo",
  "kilogram",   "kg",
  "gram",       "gram",
  "gram",       "gm",
  "gram",       "gr",
  "gram",       "g",
  "pound",      "pound",
  "pound",      "lb",
  "ounce",      "ounce",
  "ounce",      "oz")

#' Convert a user-provided weight to kilograms, totaling several weights if
#' needed.
#'
#' @param raw_text The exact string provided by the user when prompted for a
#'   child's weight
#' @return A number of kilograms equivalent to the given weight, with an
#'   `explained` character attribute with a standardized human-readable
#'   interpretation of the input
#' @examples
#' standardize_weight("3930 g")
#' standardize_weight("8 lb 10 oz")
standardize_weight <- function(raw_text) {
  with_digits <- raw_text %>%
    str_to_lower() %>%
    str_replace_all(NUMBERS_REPLACEMENTS)
  weight_pattern <- regex("([0-9.]+)\\s*([a-z]+)", ignore_case = TRUE)
  weights_raw <- with_digits %>%
    str_match_all(weight_pattern) %>%
    .[[1]]
  colnames(weights_raw) <- c("full", "amount", "unit")
  weights_raw <- weights_raw %>%
    as_tibble() %>%
    transmute(
      amount = as.numeric(amount),
      unit_raw = str_remove(unit, "[sS]$"))
  weights_parsed <- weights_raw %>%
    inner_join(WEIGHT_ABBREVIATIONS, by = c("unit_raw" = "abbr")) %>%
    inner_join(WEIGHT_LOOKUPS, by = "unit")
  total_weight <- weights_parsed %>%
    pmap_dfr(function(...) {
      parsed_row <- tibble(...)
      row_kg <- parsed_row$amount %>%
        as_units(parsed_row$udunits_key) %>%
        set_units("kilogram") %>%
        drop_units()
      parsed_row %>%
        mutate(row_kg = row_kg)
    }) %>%
    summarize(total_kg = sum(row_kg)) %>%
    .$total_kg
  attr(total_weight, "explained") <- weights_parsed %>%
    arrange(desc(kg_equiv)) %>%
    pmap_chr(function(...) {
      weight <- tibble(...)
      str_c(weight$amount, " ", weight$canonical)
    }) %>%
    str_c(collapse = " ")
  total_weight
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
  ~unit,    ~plural,  ~days,
  "day",    "days",   1,
  "week",   "weeks",  7,
  # For growth curves, "1 month = 30.4375 days" (i.e., 365.25 / 12), per:
  # https://cdn.who.int/media/docs/default-source/child-growth/child-growth-standards/indicators/weight-for-age/instructionsa1066ff7424c40fa9282d0e6623dc097.pdf?sfvrsn=334a2f9a_0
  "month",  "months", 30.4375,
  "year",   "years",  365.25)
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
#' @return A number of days equivalent to the given age, with an `explained`
#'   character attribute with a standardized human-readable interpretation of
#'   the input
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
  ages_parsed <- ages_raw %>%
    inner_join(AGE_ABBREVIATIONS, by = c("unit_raw" = "abbr")) %>%
    inner_join(AGE_LOOKUPS, by = "unit")
  if (nrow(ages_parsed) == 0) {
    stop("Unable to interpret age: ", raw_text)
  } else {
    total_age <- ages_parsed %>%
      summarize(total_days = sum(amount * days)) %>%
      .$total_days
    attr(total_age, "explained") <- ages_parsed %>%
      arrange(desc(days)) %>%
      pmap_chr(function(...) {
        age <- tibble(...)
        str_c(age$amount, " ", ifelse(age$amount == 1, age$unit, age$plural))
      }) %>%
      str_c(collapse = ", ")
    total_age
  }
}

# LENGTH STANDARDIZATION ------------------------------------------------------

LENGTH_LOOKUPS <- tribble(
    ~unit,          ~canonical,   ~udunits_key,
    "centimeter",   "cm",         "centimeter",
    "inch",         "in",         "international_inch",
    "foot",         "ft",         "US_survey_foot",
    "yard",         "yd",         "US_survey_yard",
    "meter",        "m",          "meter") %>%
  pmap_dfr(function(...) {
    lookup <- tibble(...)
    cm_equiv <- 1.0 %>%
      as_units(lookup$udunits_key) %>%
      set_units("centimeter") %>%
      drop_units()
    lookup %>%
      mutate(cm_equiv = cm_equiv)
  })
LENGTH_ABBREVIATIONS <- tribble(
  ~unit,          ~abbr,
  "centimeter",   "centimeter",
  "centimeter",   "cm",
  "centimeter",   "centimetre",
  "inch",         "inch",
  "inch",         "in",
  "inch",         "inche",
  "inch",         "\"",
  "inch",         "\u201c",       # U+201C LEFT DOUBLE QUOTATION MARK
  "inch",         "\u201d",       # U+201D RIGHT DOUBLE QUOTATION MARK
  "inch",         "\u201e",       # U+201E DOUBLE LOW-9 QUOTATION MARK
  "inch",         "\u201f",       # U+201F DOUBLE HIGH-REVERSED-9 QUOTATION MARK
  "inch",         "\u2e42",       # U+2E42 DOUBLE LOW-REVERSED-9 QUOTATION MARK
  "foot",         "foot",
  "foot",         "ft",
  "foot",         "'",
  "foot",         "\u2018",       # U+2018 LEFT SINGLE QUOTATION MARK
  "foot",         "\u2019",       # U+2019 RIGHT SINGLE QUOTATION MARK
  "foot",         "\u201a",       # U+201A SINGLE LOW-9 QUOTATION MARK
  "foot",         "\u201b",       # U+201B SINGLE HIGH-REVERSED-9 QUOTATION MARK
  "yard",         "yard",
  "yard",         "yd",
  "meter",        "meter",
  "meter",        "m",
  "meter",        "metre")

#' Convert a user-provided length to kilograms, totaling several lengths if
#' needed.
#'
#' @param raw_text The exact string provided by the user when prompted for a
#'   child's length
#' @return A number of centimeters equivalent to the given length, with an
#'   `explained` character attribute with a standardized human-readable
#'   interpretation of the input
#' @examples
#' standardize_length("25 inches")
#' standardize_length("2'0\"")
#' standardize_length("60cm")
standardize_length <- function(raw_text) {
  with_digits <- raw_text %>%
    str_to_lower() %>%
    str_replace_all(NUMBERS_REPLACEMENTS)
  length_pattern <- regex(
    "([0-9.]+)\\s*([a-z\\p{Quotation_Mark}]+)",
    ignore_case = TRUE)
  lengths_raw <- with_digits %>%
    str_match_all(length_pattern) %>%
    .[[1]]
  colnames(lengths_raw) <- c("full", "amount", "unit")
  lengths_raw <- lengths_raw %>%
    as_tibble() %>%
    transmute(
      amount = as.numeric(amount),
      unit_raw = str_remove(unit, "[sS]$"))
  lengths_parsed <- lengths_raw %>%
    inner_join(LENGTH_ABBREVIATIONS, by = c("unit_raw" = "abbr")) %>%
    inner_join(LENGTH_LOOKUPS, by = "unit")
  total_length <- lengths_parsed %>%
    pmap_dfr(function(...) {
      parsed_row <- tibble(...)
      row_cm <- parsed_row$amount %>%
        as_units(parsed_row$udunits_key) %>%
        set_units("centimeter") %>%
        drop_units()
      parsed_row %>%
        mutate(row_cm = row_cm)
    }) %>%
    summarize(total_cm = sum(row_cm)) %>%
    .$total_cm
  attr(total_length, "explained") <- lengths_parsed %>%
    arrange(desc(cm_equiv)) %>%
    pmap_chr(function(...) {
      length <- tibble(...)
      str_c(length$amount, " ", length$canonical)
    }) %>%
    str_c(collapse = " ")
  total_length
}
