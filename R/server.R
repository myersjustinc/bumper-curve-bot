# DEPENDENCIES ----------------------------------------------------------------
library(tidyverse)
library(beakr)
library(pool)

# CONSTANTS -------------------------------------------------------------------
FLAT_HTML_ROOT <- here::here("extdata", "html")
STATIC_FILE_ROOT <- here::here("extdata", "static")

# APPLICATION SOURCE ----------------------------------------------------------

# TODO: Load whatever needs loading.

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
#' @param db A DBI::DBIConnection.
#' @return Nothing
start_server <- function(port, db) {
  newBeakr() %>%
    httpGET("/", serve_flat_html("home.html")) %>%
    httpGET("/terms", serve_flat_html("terms.html")) %>%
    # TODO: Define installation helper (redirect).
    # TODO: Define interaction webhook endpoint.
    serveStaticFiles("/static", STATIC_FILE_ROOT) %>%
    handleErrors() %>%
    listen(host = "0.0.0.0", port = port)
}
