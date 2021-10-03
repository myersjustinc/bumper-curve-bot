# DEPENDENCIES ----------------------------------------------------------------
library(tidyverse)
library(beakr)

# CONSTANTS -------------------------------------------------------------------
FLAT_HTML_ROOT <- here::here("extdata", "html")
STATIC_FILE_ROOT <- here::here("extdata", "static")

# APPLICATION SOURCE ----------------------------------------------------------
source(here::here("R", "auth.R"))
source(here::here("R", "discord_helpers.R"))
source(here::here("R", "http_handlers.R"))

# SETUP HELPERS ---------------------------------------------------------------

#' Serve an HTML file.
#'
#' @param path A string path relative to STATIC_FILE_ROOT.
#' @return A beakr middleware function
serve_flat_html <- function(path) {
  abs_path <- normalizePath(file.path(FLAT_HTML_ROOT, path))
  path_is_safe <- str_starts(
    abs_path,
    fixed(FLAT_HTML_ROOT, ignore_case = FALSE))
  function(req, res, err) {
    res$setStatus(if_else(path_is_safe, 200, 404))
    res$setContentType("text/html")
    res$setBody(read_file(if_else(
      path_is_safe, abs_path, file.path(FLAT_HTML_ROOT, "404.html"))))
  }
}

# ROUTES ----------------------------------------------------------------------

#' Start the web server itself.
#'
#' @param port An integer port on which to listen.
#' @return Nothing
start_server <- function(port) {
  newBeakr() %>%
    httpGET("/", serve_flat_html("home.html")) %>%
    httpGET("/terms", serve_flat_html("terms.html")) %>%
    httpGET("/install", function(req, res, err) {
      access_token <- discord_token()
      register_slash_command("curve", CLIENT_ID, access_token)
      res$redirect(INSTALL_URL)
    }) %>%
    httpGET("/interaction", http_post_interaction) %>%
    serveStaticFiles("/static", STATIC_FILE_ROOT) %>%
    handleErrors() %>%
    listen(host = "0.0.0.0", port = port)
}
