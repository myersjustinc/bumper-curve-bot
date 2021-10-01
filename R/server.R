# DEPENDENCIES ----------------------------------------------------------------
library(tidyverse)
library(beakr)
library(pool)

# APPLICATION SOURCE ----------------------------------------------------------

# TODO: Load whatever needs loading.

# SETUP HELPERS ---------------------------------------------------------------

# TODO: Add these.

# ROUTES ----------------------------------------------------------------------

#' Start the web server itself.
#'
#' @param port An integer port on which to listen.
#' @param db A DBI::DBIConnection.
#' @return Nothing
start_server <- function(port, db) {
  newBeakr() %>%
    # TODO: Define home page.
    # TODO: Define terms-and-policies page.
    # TODO: Define OAuth redirect endpoint.
    # TODO: Define interaction webhook endpoint.
    handleErrors() %>%
    listen(host = "0.0.0.0", port = port)
}
