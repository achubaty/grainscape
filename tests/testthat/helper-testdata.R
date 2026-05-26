## Shared test fixtures built once per session using withr::local_* in each test,
## or accessed directly via the helpers below.

.tinyMPG <- function() {
  tiny <- terra::rast(system.file("extdata/tiny.asc", package = "grainscape"))
  tinyCost <- terra::classify(tiny, rcl = cbind(c(1, 2, 3, 4), c(1, 5, 10, 12)))
  MPG(cost = tinyCost, patch = tinyCost == 1)
}

.tinyGOC <- function(mpg = .tinyMPG(), nThresh = 5) {
  GOC(mpg, nThresh = nThresh)
}
