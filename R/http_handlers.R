# DEPENDENCIES ----------------------------------------------------------------
library(tidyverse)
library(jsonlite)

# LOAD HELPERS ----------------------------------------------------------------
source(here::here("R", "discord_helpers.R"))
source(here::here("R", "curves.R"))
source(here::here("R", "output_helpers.R"))

# INTERACTION TYPES -----------------------------------------------------------

#' Handle an APPLICATION_COMMAND interaction from a Discord webhook.
#'
#' @param req A beakr::Request.
#' @param res A beakr::Response.
#' @param err A beakr::Error.
#' @return A beakr::Response
http_post_interaction_application_command <- function(req, res, err) {
  parsed <- fromJSON(req$body)
  message(str_c("Incoming application command:\n", req$body))
  name <- parsed$data$name
  options <- parsed$data$options
  if (name == "curve") {
    if (options$name == "help") {
      message <- str_c(
        'Use this command to look up where your child lands on the WHO ',
        'growth curves at a particular age. You can use `/curve weight`, ',
        '`/curve length` or `/curve head`.',
        '\n\n',
        'You\'ll be asked for three details about your child:\n',
        '\u2022 Their sex (male or female, since those are the only ',
                'versions of the curves available)\n',
        '\u2022 The measurement you\'re interested in (weight, ',
                'length/height, head circumference)\n',
        '\u2022 Their age',
        '\n\n',
        'We\'re pretty forgiving about how you write the ages and other ',
        'measurements, but if you think the bot is misunderstanding ',
        'something, ask @myersjustinc#0042.'
        )
    } else {
      curve_options <- parse_options_curve(options)
      message <- case_when(
        !curve_options$is_valid ~ curve_options$error,
        TRUE ~ switch(
          curve_options$type,
          weight = format_curve(
            percentile_weight(
              curve_options$sex, curve_options$weight, curve_options$age),
            curve_options),
          length = format_curve(
            percentile_length(
              curve_options$sex, curve_options$length, curve_options$age),
            curve_options),
          head = format_curve(
            percentile_head(
              curve_options$sex, curve_options$circumference, curve_options$age),
            curve_options)
          ))
    }
  } else {
    message <- "I didn't understand that."
  }

  res$setStatus(200)
  res$setHeader("Content-Type", "application/json")
  tribble(
      ~type,  ~data,
      4,      unbox(tribble(
        ~flags,             ~content,
        bitwShiftL(1, 6),   message))) %>%
    unbox() %>%
    toJSON() %>%
    as.character() %>%
    res$setBody()
}

#' Handle a MESSAGE_COMPONENT interaction from a Discord webhook.
#'
#' @param req A beakr::Request.
#' @param res A beakr::Response.
#' @param err A beakr::Error.
#' @return A beakr::Response
http_post_interaction_message_component <- function(req, res, err) {
  res$setStatus(200)
  res$setHeader("Content-Type", "application/json")
  tribble(
      ~type,
      6) %>%  # acknowledge, but don't act
    unbox() %>%
    toJSON() %>%
    as.character() %>%
    res$setBody()
}

#' Handle a PING interaction from a Discord webhook.
#'
#' @param req A beakr::Request.
#' @param res A beakr::Response.
#' @param err A beakr::Error.
#' @return A beakr::Response
http_post_interaction_ping <- function(req, res, err) {
  res$setStatus(200)
  res$setHeader("Content-Type", "application/json")
  tribble(
      ~type,
      1) %>%
    unbox() %>%
    toJSON() %>%
    as.character() %>%
    res$setBody()
}

#' Handle unknown types of interaction from a Discord webhook.
#'
#' @param req A beakr::Request.
#' @param res A beakr::Response.
#' @param err A beakr::Error.
#' @return A beakr::Response
http_post_interaction_unknown <- function(req, res, err) {
  res$setStatus(401)
  res$setHeader("Content-Type", "application/json")
  tribble(
      ~error,
      "unknown interaction type") %>%
    unbox() %>%
    toJSON() %>%
    as.character() %>%
    res$setBody()
}

# /interaction DISPATCHER -----------------------------------------------------

#' Handle an interaction from a Discord webhook.
#'
#' @param req A beakr::Request.
#' @param res A beakr::Response.
#' @param err A beakr::Error.
#' @return A beakr::Response
http_post_interaction <- function(req, res, err) {
  public_key <- Sys.getenv("DISCORD_PUBLIC_KEY")
  if (!verify_request_signature(req, public_key)) {
    res$setStatus(401)
    res$setHeader("Content-Type", "application/json")
    tribble(
        ~error,
        "invalid request signature") %>%
      unbox() %>%
      toJSON() %>%
      as.character() %>%
      res$setBody()
  } else {
    parsed <- fromJSON(req$body)
    interaction_type <- parsed$type
    if (interaction_type == 1) {
      http_post_interaction_ping(req, res, err)
    } else if (interaction_type == 2) {
      http_post_interaction_application_command(req, res, err)
    } else if (interaction_type == 3) {
      http_post_interaction_message_component(req, res, err)
    } else {
      http_post_interaction_unknown(req, res, err)
    }
  }
}
