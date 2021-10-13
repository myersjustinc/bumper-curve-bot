# DEPENDENCIES ----------------------------------------------------------------
library(tidyverse)
library(sodium)

# LOAD HELPERS ----------------------------------------------------------------
source(here::here("R", "arguments.R"))

# OTHER UTILITY FUNCTIONS -----------------------------------------------------

#' Convert a hexadecimal string of arbitrary length to a raw object containing
#' the repreaented bytes.
#'
#' @param hex_string A string of hexadecimal digits.
#' @return A raw object
#' @examples
#' hex_to_raw("0badf00d")
#' hex_to_raw("6d61646520796f75206c6f6f6b")
hex_to_raw <- function(hex_string) {
  hex_string %>%
    str_extract_all("[0-9a-fA-F]{2}") %>%
    .[[1]] %>%
    strtoi(16) %>%
    as.raw()
}

#' Generate a human-readable error message to explain an unparseable argument.
#'
#' @param name A string argument name that should match what the user sees in
#'   Discord.
#' @param value A string identical to the user's provided value for that
#'   argument.
#' @return A string error message
#' @examples
#' build_error("age", "one fortnight")
build_error <- function(name, value) {
  str_c(
    "I couldn't understand your provided `", name, "` of ",
    "`", attr(value, "raw"), "`."
  )
}

# DISCORD API INTERACTIONS ----------------------------------------------------

#' Parse options for the `/curve` command.
#'
#' @param options A two-dimensional list such as the one returned in fromJSON,
#'   with the $options property being a list whose one element is a
#'   two-dimensional list of options.
#' @return A list with six properties: string $type, logical $is_valid,
#'   string $error, string $sex, numeric $age, and one of the following numeric
#'   properties: $weight, $length, $circumference
#' @examples
#' parse_options_curve(list(
#'   name = "weight",
#'   type = 1,
#'   options = list(list(
#'     name = c("sex", "weight", "age"),
#'     type = 3,
#'     value = c("F", "11lb13.4oz", "1m")))))
parse_options_curve <- function(options) {
  command_name <- options$name
  options <- as_tibble(options$options[[1]]) %>%
    select(name, value) %>%
    deframe()

  sex_parsed <- tryCatch(
    { standardize_sex(options[["sex"]]) },
    error = function(unused) {
      out <- NA_real_
      attr(out, "raw") <- options[["sex"]]
      out
    })
  age_parsed <- tryCatch(
    { standardize_age(options[["age"]]) },
    error = function(unused) {
      out <- NA_real_
      attr(out, "raw") <- options[["age"]]
      out
    })
  output <- list(
    "type" = command_name,
    "is_valid" = case_when(
      is.na(sex_parsed) ~ FALSE,
      is.na(age_parsed) ~ FALSE,
      TRUE ~ TRUE),
    "error" = case_when(
      is.na(sex_parsed) ~ build_error("sex", sex_parsed),
      is.na(age_parsed) ~ build_error("age", age_parsed),
      TRUE ~ NA_character_),
    "sex" = sex_parsed,
    "age" = age_parsed)

  if (command_name == "weight") {
    weight_parsed <- tryCatch(
      { standardize_weight(options[["weight"]]) },
      error = function(unused) {
        out <- NA_real_
        attr(out, "raw") <- options[["weight"]]
        out
      })
    output[["weight"]] <- weight_parsed
    if (is.na(weight_parsed)) {
      output[["is_valid"]] <- FALSE
      output[["error"]] <- build_error("weight", weight_parsed)
    }
  } else if (command_name == "length") {
    length_parsed <- tryCatch(
      { standardize_length(options[["length"]]) },
      error = function(unused) {
        out <- NA_real_
        attr(out, "raw") <- options[["length"]]
        out
      })
    output[["length"]] <- length_parsed
    if (is.na(length_parsed)) {
      output[["is_valid"]] <- FALSE
      output[["error"]] <- build_error("length", length_parsed)
    }
  } else if (command_name == "head") {
    circumference_parsed <- tryCatch(
      { standardize_length(options[["circumference"]]) },
      error = function(unused) {
        out <- NA_real_
        attr(out, "raw") <- options[["circumference"]]
        out
      })
    output[["circumference"]] <- circumference_parsed
    if (is.na(circumference_parsed)) {
      output[["is_valid"]] <- FALSE
      output[["error"]] <- build_error("circumference", circumference_parsed)
    }
  }

  output
}

#' Register a slash command with Discord.
#'
#' @param command_name A string naming the command to create or update.
#' @param app_id A string application ID.
#' @param client_token A string authentication token from a valid set of client
#'   credentials.
#' @param guild_id An optional string ID for a specific Discord server for
#'   which to install or update this command. If omitted, all installations wi
#'   be affected.
#' @return Nothing
#' @examples
#' percentile_weight("F", 3.930, 0)
register_slash_command <- function(
    command_name, app_id, client_token, guild_id = NA_character_) {
  command_meta_url <- str_c(
    "https://discord.com/api/v8/applications/", app_id,
    ifelse(is.na(guild_id), "", str_c("/guilds/", guild_id)),
    "/commands")
  command_spec_path <- here::here("extdata", str_c(
    "def_", command_name, ".json"))
  res <- POST(
    command_meta_url,
    add_headers(Authorization = str_c("Bearer ", client_token)),
    body = upload_file(command_spec_path, type = "application/json"))
  stop_for_status(res)
}

#' Verify the Ed25519 signature of an incoming Discord interaction.
#'
#' @param req A beakr::Request.
#' @param public_key_hex A string Ed25519 public key, represented in
#'   hexadecimal.
#' @return A logical noting whether the signature is valid
verify_request_signature <- function(req, public_key_hex) {
  tryCatch(
    (function() {
      message <- str_c(
          req$headers$x_signature_timestamp,
          req$body) %>%
        charToRaw()
      signature <- hex_to_raw(req$headers$x_signature_ed25519)
      public_key <- hex_to_raw(public_key_hex)
      tryCatch(
        sig_verify(message, signature, public_key),
        error = function(unused) { FALSE })
    })(),
    error = function(unused) { FALSE })
}
