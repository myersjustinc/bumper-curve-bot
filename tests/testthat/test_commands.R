context("Discord API objects")
test_dir <- utils::getSrcDirectory(function(foo) { foo })
source(file.path(test_dir, "..", "..", "R", "commands.R"))

test_that("/curve options get parsed properly", {
  raw_options <- list(
    name = c("sex", "weight", "age"),
    value = c("F", "11lb13.4oz", "1m"))
  expect_equal(
    parse_options_curve(raw_options),
    list(
      sex = "F",
      weight = structure(5.3694, explained = "11 lb 13.4 oz"),
      age = structure(30.4375, explained = "1 month")),
    tolerance = 0.0001)
})
