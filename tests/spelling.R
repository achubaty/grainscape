if (!isTRUE(Sys.getenv("CI")) && requireNamespace("spelling", quietly = TRUE))
  spelling::spell_check_test(vignettes = TRUE, error = FALSE, skip_on_cran = TRUE)
