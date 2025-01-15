withr::local_options(list(
  warnPartialMatchArgs = TRUE,
  warnPartialMatchAttr = TRUE,
  warnPartialMatchDollar = TRUE
))

library(testthat)

test_check("grainscape")
