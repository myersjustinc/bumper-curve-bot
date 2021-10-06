# bumperbot #

This R application provides a [Discord slash command][] to let users calculate
percentiles for children's growth using the
[World Health Organization's growth standards][].

It relies heavily on [`beakr`][] (for creating an R-based web application),
[`UDUNITS`][] (for flexibility in units of measurement) and [`anthro`][] (for
making the WHO standards easy to use).

## Why? ##

Because I'm in a Discord group with a bunch of other parents of young children.

## No, but why R? ##

The `anthro` package is really nice, promoted by WHO itself and written in R.
So if I wanted to use it in a bot, I'd either need to shell out to an R script
from my non-R application (with considerable resulting overhead) or simply
build the whole thing in R.

## Usage ##

You can build a Docker image from this repo, or you can refer to the
`Dockerfile` for some rough guidance on what dependencies you'd need to install
outside of a containerized environment. Either way, `/run.R` will start the
server.

You'll need three pieces of information from your [Discord application][], all
of which can be provided through environment variables:

*   `DISCORD_CLIENT_ID`
*   `DISCORD_CLIENT_SECRET`
*   `DISCORD_PUBLIC_KEY`
