test_that("plotResistance returns a buildable ggplot", {
  withr::local_options(
    warnPartialMatchArgs = TRUE,
    warnPartialMatchAttr = TRUE,
    warnPartialMatchDollar = TRUE
  )
  skip_if_not_installed("ggplot2")

  tiny <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))

  p <- plotResistance(tinyCost)
  expect_s3_class(p, "ggplot")
  expect_no_error(ggplot2::ggplot_build(p))

  ## maxResistance is honoured / auto-raised so no cell value is dropped to NA
  built <- ggplot2::ggplot_build(plotResistance(tinyCost, maxResistance = 5))
  expect_false(any(is.na(built$data[[1]]$fill)))
})

test_that("plotWithResistance pairs a resistance panel with a network plot", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("cowplot")

  tiny <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  tinyMPG <- MPG(tinyCost, patch = (tinyCost == 1))

  g <- plotWithResistance(tinyCost, plot(tinyMPG, quick = "mpgPlot", theme = FALSE))
  expect_s3_class(g, "gg")
  expect_no_error(ggplot2::ggplot_build(g))
})
